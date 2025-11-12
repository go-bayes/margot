#' Format a Transition Table with observed‑indicator filtering
#'
#' Reshapes a long‑format transition frequency data frame into a
#' wide‑format table with totals, and formats it using markdown.
#' Diagonal elements are bolded to highlight state stability.
#'
#' @param data a data frame containing your id, state, wave, and (optionally) an observed indicator.
#' @param state_var name of the column indicating the state at each wave.
#' @param id_var    name of the column identifying each participant.
#' @param wave_var  name of the column indicating the wave (numeric or factor).
#' @param waves     optional numeric vector of waves to include (defaults to all present in data).
#' @param state_names optional character vector of labels for each state.
#' @param observed_var optional name of a column marking participants still observed.
#' @param observed_val value in \code{observed_var} that denotes “observed” (default 1).
#' @param table_name name for the output object (default "transition_table").
#'
#' @return an object of class \code{margot_transitions} with
#'   \code{tables} (markdown-formatted matrices), paired \code{tables_data}
#'   (the underlying numeric data frames), \code{explanation}, \code{wave_info},
#'   and a \code{quarto_code} helper.
#' @export
margot_transition_table <- function(data, state_var, id_var, wave_var,
                                    waves = NULL, state_names = NULL,
                                    observed_var = NULL, observed_val = 1,
                                    table_name = "transition_table") {
  # filter by observed indicator if requested
  if (!is.null(observed_var)) {
    if (!observed_var %in% names(data)) {
      stop(sprintf("observed_var '%s' not found in data", observed_var))
    }
    data <- data[data[[observed_var]] == observed_val, , drop = FALSE]
  }

  if (is.null(waves)) {
    waves <- sort(unique(data[[wave_var]]))
  }
  waves <- sort(waves)

  results <- list(
    tables = list(),
    tables_data = list(),
    waves = list(),
    table_name = table_name
  )
  results$explanation <- paste0(
    "These transition matrices capture shifts in states between consecutive waves. ",
    "Each cell shows the count of individuals transitioning from one state to another. ",
    "Rows are the initial state (From), columns the subsequent state (To). ",
    "**Diagonal entries** (in **bold**) mark those who stayed in the same state."
  )

  for (i in seq_len(length(waves) - 1)) {
    w1 <- waves[i]
    w2 <- waves[i + 1]

    dat_pair <- data[data[[wave_var]] %in% c(w1, w2), , drop = FALSE]
    tmp <- data.frame(
      id    = dat_pair[[id_var]],
      wave  = dat_pair[[wave_var]],
      state = dat_pair[[state_var]]
    )

    wide <- reshape(tmp, idvar = "id", timevar = "wave", direction = "wide")
    from_col <- paste0("state.", w1)
    to_col <- paste0("state.", w2)

    ok <- !is.na(wide[[from_col]]) & !is.na(wide[[to_col]])
    from_states <- wide[[from_col]][ok]
    to_states <- wide[[to_col]][ok]

    transition_matrix <- table(from = from_states, to = to_states)

    # skip empty transitions
    if (nrow(transition_matrix) == 0 || ncol(transition_matrix) == 0) {
      warning("no observed transitions for waves ", w1, "→", w2, "; skipping")
      next
    }

    all_states <- sort(unique(c(rownames(transition_matrix), colnames(transition_matrix))))
    labels <- if (is.null(state_names)) paste0("State ", all_states) else state_names

    trans_df <- as.data.frame.matrix(transition_matrix)
    trans_df$from <- rownames(trans_df)
    long_df <- tidyr::pivot_longer(trans_df,
      cols = -from,
      names_to = "to", values_to = "Freq"
    )

    res <- transition_table(long_df,
      state_names = labels,
      wave_info   = paste0("Wave ", w1, " → Wave ", w2),
      table_name  = table_name
    )

    results$tables[[i]] <- res$table
    results$tables_data[[i]] <- res$table_data
    results$waves[[i]] <- c(w1, w2)
  }

  results$quarto_code <- function() {
    cat(sprintf("```{r, results='asis'}\ncat(%s$explanation)\n```\n\n", table_name))
    for (i in seq_along(results$tables)) {
      w1 <- results$waves[[i]][1]
      w2 <- results$waves[[i]][2]
      cat(sprintf(
        "```{r}\n#| label: %s-wave%s-%s\n#| tbl-cap: \"Transition Matrix: Wave %s → %s\"\n%s$tables[[%d]]\n```\n\n",
        table_name, w1, w2, w1, w2, table_name, i
      ))
    }
  }

  class(results) <- c("margot_transitions", class(results))
  results
}

#' Format Transition Table
#'
#' Helper function to format a single transition data frame into a markdown table.
#'
#' @param trans_df A data frame with columns 'from', 'to', and 'Freq'
#' @param state_names Optional vector of state names
#' @param wave_info Optional wave information string
#' @param table_name Name for the table (default "transition_table")
#'
#' @return A list with formatted table and explanation
#' @keywords internal
#' @noRd
transition_table <- function(trans_df, state_names = NULL,
                             wave_info = NULL, table_name = "transition_table") {
  required_cols <- c("from", "to", "Freq")
  if (!all(required_cols %in% colnames(trans_df))) {
    stop("trans_df must contain columns: from, to, and Freq")
  }

  trans_wide <- tidyr::pivot_wider(
    trans_df,
    id_cols     = "from",
    names_from  = "to",
    values_from = "Freq",
    values_fill = 0
  )
  trans_wide$Total <- rowSums(trans_wide[, -1], na.rm = TRUE)
  colnames(trans_wide)[1] <- "From / To"

  if (!is.null(state_names)) {
    mapping <- setNames(state_names, sort(unique(c(trans_df$from, trans_df$to))))
    trans_wide[["From / To"]] <- mapping[as.character(trans_wide[["From / To"]])]
    for (j in 2:(ncol(trans_wide) - 1)) {
      old <- colnames(trans_wide)[j]
      if (old %in% names(mapping)) colnames(trans_wide)[j] <- mapping[old]
    }
  }

  formatted <- knitr::kable(trans_wide, format = "markdown")
  attr(formatted, "table_data") <- trans_wide
  # bold diagonal
  for (r in seq_len(nrow(trans_wide))) {
    state <- trans_wide[r, "From / To"]
    idx <- which(colnames(trans_wide) == state)
    if (length(idx)) {
      val <- trans_wide[r, idx]
      # fix escape: double backslashes for regex
      pattern <- paste0("\\|\\s*", val, "\\s*\\|")
      replacement <- paste0("| **", val, "** |")
      formatted <- gsub(pattern, replacement, formatted)
    }
  }

  if (!is.null(wave_info)) {
    header <- paste0("### ", wave_info, "\n\n")
    formatted_with_hdr <- paste0(header, formatted)
    attr(formatted_with_hdr, "table_data") <- trans_wide
    return(list(
      table             = formatted,
      table_with_header = formatted_with_hdr,
      wave_info         = wave_info,
      table_name        = table_name,
      table_data        = trans_wide
    ))
  }
  list(table = formatted, table_name = table_name, table_data = trans_wide)
}
