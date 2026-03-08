#' Assemble LMTP learner diagnostics for reporting
#'
#' Bundles the learner heatmap, concise narrative, and a compact summary table
#' of dominant learners for a selected outcome and set of shifts. Designed to
#' keep the visual and textual learner diagnostics aligned in Quarto and
#' manuscript workflows.
#'
#' @param x Result of `margot_lmtp()` (with `$models`) or another object
#'   accepted by the learner helpers.
#' @param outcome Optional character outcome name. When `NULL`, the first
#'   stored outcome is used.
#' @param shifts Optional character vector of shifts to include (full or cleaned
#'   names). If `NULL`, all available shifts are used.
#' @param label_mapping Optional label map passed through to downstream helpers.
#' @param waves Optional integer vector selecting waves.
#' @param remove_waves Optional integer vector of waves to drop after subsetting.
#' @param title Optional plot title passed to [margot_plot_lmtp_learners()].
#' @param component Which nuisance models to include: `"both"` (default),
#'   `"outcome"` (only `m`), or `"treatment"` (only `r`).
#' @param digits Integer rounding applied to percentage columns in the summary
#'   table and to the narrative produced by
#'   [margot_interpret_lmtp_learners()].
#' @param include_plot Logical; if `TRUE`, returns a `ggplot2` object from
#'   [margot_plot_lmtp_learners()].
#' @param plot_args Optional named list overriding defaults passed to
#'   [margot_plot_lmtp_learners()].
#' @param interpret_args Optional named list overriding defaults passed to
#'   [margot_interpret_lmtp_learners()] (for example, `digits = 1`).
#'
#' @return A named list with elements:
#'   - `summary_table`: compact dominant-learner table by shift, component, and wave.
#'   - `learner_data`: tidy learner-weight data from `summarise_lmtp_learners()`.
#'   - `plot`: learner heatmap (or `NULL` when `include_plot = FALSE`).
#'   - `narrative`: structured list from `margot_interpret_lmtp_learners(return = "list")`.
#'   - `method_statement`: single character string describing the learner diagnostics.
#'   - `metadata`: list of context (outcome, shifts, waves, component).
#' @export
margot_report_lmtp_learners <- function(x,
                                        outcome = NULL,
                                        shifts = NULL,
                                        label_mapping = NULL,
                                        waves = NULL,
                                        remove_waves = NULL,
                                        title = NULL,
                                        component = c("both", "outcome", "treatment"),
                                        digits = 1,
                                        include_plot = TRUE,
                                        plot_args = list(),
                                        interpret_args = list()) {
  component <- match.arg(component)
  if (!is.null(outcome)) stopifnot(is.character(outcome), length(outcome) == 1L)
  if (!is.null(shifts)) stopifnot(is.character(shifts))

  selection <- margot_resolve_positivity_selection(
    x = x,
    outcome = outcome,
    shifts = shifts,
    caller = "margot_report_lmtp_learners"
  )
  outcome <- selection$outcome
  selected_shifts <- selection$shift_df$shift_full

  learner_data <- summarise_lmtp_learners(
    x = x,
    outcome = outcome,
    shifts = selected_shifts,
    label_mapping = label_mapping,
    waves = waves,
    remove_waves = remove_waves
  )

  if (!nrow(learner_data)) {
    stop("No learner weights available for the requested configuration.", call. = FALSE)
  }

  if (!identical(component, "both")) {
    learner_data <- learner_data[learner_data$component == component, , drop = FALSE]
  }
  if (!nrow(learner_data)) {
    stop("No learner weights available after filtering components.", call. = FALSE)
  }

  summary_table <- build_lmtp_learner_summary_table(
    learner_data = learner_data,
    digits = digits
  )

  plot <- NULL
  if (isTRUE(include_plot)) {
    plot_defaults <- list(
      x = x,
      outcome = outcome,
      shifts = selected_shifts,
      label_mapping = label_mapping,
      waves = waves,
      remove_waves = remove_waves,
      title = title,
      component = component
    )
    plot_call <- utils::modifyList(plot_defaults, plot_args)
    plot <- do.call(margot_plot_lmtp_learners, plot_call)
  }

  interpret_defaults <- list(
    x = x,
    outcome = outcome,
    shifts = selected_shifts,
    label_mapping = label_mapping,
    waves = waves,
    remove_waves = remove_waves,
    component = component,
    digits = digits,
    return = "list"
  )
  interpret_call <- utils::modifyList(interpret_defaults, interpret_args)
  interpret_call$return <- "list"
  narrative <- do.call(margot_interpret_lmtp_learners, interpret_call)
  if (!is.list(narrative)) {
    narrative <- list(text = narrative)
  }

  method_statement <- build_lmtp_learner_method_statement(component = component)

  list(
    summary_table = summary_table,
    learner_data = if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
      tibble::as_tibble(learner_data)
    } else {
      learner_data
    },
    plot = plot,
    narrative = narrative,
    method_statement = method_statement,
    metadata = list(
      outcome = outcome,
      shifts = selected_shifts,
      waves = waves,
      remove_waves = remove_waves,
      component = component,
      digits = digits,
      title = title
    )
  )
}

build_lmtp_learner_summary_table <- function(learner_data, digits = 1) {
  if (is.null(learner_data) || !nrow(learner_data)) {
    return(NULL)
  }

  digits <- max(0L, as.integer(digits)[1])
  summary_rows <- list()

  shift_levels <- unique(as.character(learner_data$shift_label))
  component_order <- c("outcome", "treatment")

  for (shift_label in shift_levels) {
    shift_data <- learner_data[as.character(learner_data$shift_label) == shift_label, , drop = FALSE]
    component_levels <- unique(shift_data$component)
    component_levels <- component_order[component_order %in% component_levels]

    for (component_name in component_levels) {
      component_data <- shift_data[shift_data$component == component_name, , drop = FALSE]
      wave_order <- unique(component_data$wave_index[order(component_data$wave_index)])

      for (wave_index in wave_order) {
        wave_data <- component_data[component_data$wave_index == wave_index, , drop = FALSE]
        if (!nrow(wave_data)) next

        wave_data$learner_label <- pretty_learner_label(wave_data$learner)
        wave_data <- wave_data[order(-wave_data$weight_mean, wave_data$learner_label), , drop = FALSE]

        top <- wave_data[1, , drop = FALSE]
        runner <- if (nrow(wave_data) >= 2L) wave_data[2, , drop = FALSE] else NULL

        summary_rows[[length(summary_rows) + 1L]] <- data.frame(
          Shift = shift_label,
          Component = as.character(component_titles(component_name))[1],
          Wave = as.character(top$wave_label[1]),
          `Top learner` = as.character(top$learner_label[1]),
          `Top weight %` = round(100 * as.numeric(top$weight_mean[1]), digits),
          `Runner-up` = if (is.null(runner)) NA_character_ else as.character(runner$learner_label[1]),
          `Runner-up %` = if (is.null(runner)) NA_real_ else round(100 * as.numeric(runner$weight_mean[1]), digits),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  out <- do.call(rbind, summary_rows)
  if (isTRUE(requireNamespace("tibble", quietly = TRUE))) tibble::as_tibble(out) else out
}

build_lmtp_learner_method_statement <- function(component = c("both", "outcome", "treatment")) {
  component <- match.arg(component)

  scope_sentence <- switch(
    component,
    both = "We report these weights for both the outcome regression (m) and the treatment or density-ratio regression (r), by intervention and wave.",
    outcome = "We report these weights for the outcome regression (m), by intervention and wave.",
    treatment = "We report these weights for the treatment or density-ratio regression (r), by intervention and wave."
  )

  out <- paste(
    "Super Learner diagnostics summarise the mean ensemble weight assigned to each base learner across cross-fitting folds.",
    scope_sentence,
    "Large weights concentrated on a single learner indicate that the ensemble relied mainly on one algorithm, whereas more diffuse weights indicate that several learners contributed materially.",
    "These diagnostics describe nuisance-model composition rather than effect size, overlap, or overall model adequacy."
  )
  out <- gsub("\\s+", " ", out)
  trimws(out)
}
