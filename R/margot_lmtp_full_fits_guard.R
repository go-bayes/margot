# guard against keeping full SuperLearner fits in lmtp models.
#
# lmtp's `.return_full_fits = TRUE` keeps every fitted learner for every fold and
# time point, which can multiply a fit's memory many times over; lmtp's default
# (`FALSE`) keeps the learner weights alone, and estimates and influence
# functions do not depend on the setting. The guard only inspects the control;
# it never rewrites the control that is passed on, so fit identities and
# checkpoint fingerprints are unchanged.

# validate the optional reason for keeping full fits
margot_lmtp_check_allow_full_fits <- function(allow_full_fits,
                                              call = rlang::caller_env()) {
  if (is.null(allow_full_fits)) {
    return(invisible(NULL))
  }
  if (!is.character(allow_full_fits) || length(allow_full_fits) != 1L ||
      is.na(allow_full_fits) || !nzchar(trimws(allow_full_fits))) {
    cli::cli_abort(
      c(
        "{.arg allow_full_fits} must be `NULL` or a single non-empty character string giving the reason.",
        "i" = "For example, {.code allow_full_fits = \"inspect learner predictions for outcome y\"}."
      ),
      class = "margot_error_invalid_input",
      call = call
    )
  }
  invisible(allow_full_fits)
}

# find the control entry among the lmtp arguments. A shortened name such as
# `contr` would be matched to `control` by R's argument matching, so it is
# refused rather than left unchecked.
margot_lmtp_control_from_defaults <- function(lmtp_defaults,
                                              call = rlang::caller_env()) {
  arg_names <- names(lmtp_defaults) %||% character()
  partial <- arg_names[nchar(arg_names) >= 3L & arg_names != "control" &
    startsWith("control", arg_names)]
  if (length(partial) > 0L) {
    cli::cli_abort(
      c(
        "Name the lmtp control {.code control} in full in {.arg lmtp_defaults}.",
        "x" = "Received the shortened name{?s} {.val {partial}}, which R would match to {.code control}.",
        "i" = "Margot checks {.code lmtp_defaults$control} before fitting, so the full name is required."
      ),
      class = "margot_error_full_fits_refused",
      call = call
    )
  }
  lmtp_defaults[["control", exact = TRUE]]
}

# refuse a control that keeps full fits unless a reason is supplied. Returns the
# resolved setting (TRUE or FALSE) invisibly.
margot_lmtp_check_full_fits <- function(control,
                                        allow_full_fits = NULL,
                                        caller = "margot_lmtp",
                                        inform = TRUE,
                                        call = rlang::caller_env()) {
  margot_lmtp_check_allow_full_fits(allow_full_fits, call = call)
  if (is.null(control)) {
    control <- list()
  }
  if (!is.list(control)) {
    cli::cli_abort(
      c(
        "The lmtp control must be a list from {.fn lmtp::lmtp_control}.",
        "x" = "Received an object of class {.cls {class(control)}}."
      ),
      class = "margot_error_invalid_input",
      call = call
    )
  }
  target <- ".return_full_fits"
  control_names <- names(control) %||% rep("", length(control))
  # a shortened name would be matched to the setting by lmtp_control(), and a
  # longer name would be matched by `control$.return_full_fits` inside lmtp
  # when the exact name is absent; neither spelling is accepted unless it is FALSE
  near <- nzchar(control_names) & control_names != target &
    (startsWith(target, control_names) | startsWith(control_names, target))
  near_not_false <- control_names[near][
    !vapply(control[near], isFALSE, logical(1))
  ]
  if (length(near_not_false) > 0L) {
    cli::cli_abort(
      c(
        "The lmtp control name{?s} {.val {near_not_false}} would be matched to {.code .return_full_fits}.",
        "x" = "{.fn {caller}} checks {.code .return_full_fits} before fitting, so the setting must be spelt in full.",
        "i" = "Build the control with {.code lmtp::lmtp_control()}, or name the setting {.code .return_full_fits}."
      ),
      class = "margot_error_full_fits_refused",
      call = call
    )
  }
  # resolve over the installed lmtp's defaults, so an absent setting takes the
  # value lmtp itself would use
  resolved <- utils::modifyList(lmtp::lmtp_control(), control)
  value <- if (target %in% control_names) {
    control[[target]]
  } else {
    resolved[[target]]
  }
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    cli::cli_abort(
      c(
        "The lmtp control setting {.code .return_full_fits} must be `TRUE` or `FALSE`.",
        "x" = "Received {.code {paste(deparse(value), collapse = ' ')}}."
      ),
      class = "margot_error_full_fits_refused",
      call = call
    )
  }
  if (isTRUE(value) && is.null(allow_full_fits)) {
    cli::cli_abort(
      c(
        "{.fn {caller}} refuses to keep full SuperLearner fits.",
        "x" = "The lmtp control sets {.code .return_full_fits = TRUE}.",
        "i" = "Full fits keep every fitted learner for every fold and time point and can need tens of GiB per outcome. Estimates and influence functions do not depend on this setting, and lmtp's default ({.code FALSE}) keeps the learner weights.",
        "i" = "Keep lmtp's default {.code .return_full_fits = FALSE}, or pass {.code allow_full_fits = \"<reason>\"} to keep the full fits deliberately."
      ),
      class = "margot_error_full_fits_refused",
      call = call
    )
  }
  if (isTRUE(inform) && !is.null(allow_full_fits)) {
    if (isTRUE(value)) {
      cli::cli_alert_warning(
        "Keeping full SuperLearner fits ({.code .return_full_fits = TRUE}) because {.arg allow_full_fits} gives the reason: {.val {allow_full_fits}}."
      )
    } else {
      cli::cli_alert_info(
        "{.arg allow_full_fits} has no effect: the lmtp control keeps {.code .return_full_fits = FALSE}."
      )
    }
  }
  invisible(value)
}
