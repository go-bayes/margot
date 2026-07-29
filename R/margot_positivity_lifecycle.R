# lifecycle helpers for the legacy positivity surface retired by margot.lmtp.
# the guide-architecture change of 29 July 2026 removed every traffic light,
# tolerance and verdict from the workflow, so the enforcement machinery is
# defunct and the positivity-named wrappers are soft-deprecated.

# the release in which the seven positivity-named wrappers began warning
margot_positivity_deprecation_when <- "1.1.013"

# the standing schedule and replacement sentence shared by every wrapper warning
margot_positivity_deprecation_details <- function() {
  c(
    "!" = paste(
      "The `margot.lmtp` package now carries the registered reporting family:",
      "seal the design with `margot.lmtp::margot_lmtp_ratio_fit()` and report the",
      "density ratios with `margot.lmtp::margot_lmtp_ratio_report()`."
    ),
    "i" = paste(
      "This wrapper warns through one full release cycle after `margot.lmtp` ships",
      "and is removed the cycle after."
    ),
    "i" = paste(
      "Verdict and status fields have already been removed from its output:",
      "the reports are density-ratio reports and carry no identification judgement."
    )
  )
}

# one deprecation warning per session for a soft-deprecated positivity wrapper.
# `what` and `with` are the "fun()" specifications lifecycle takes.
margot_deprecate_positivity <- function(what, with = NULL) {
  lifecycle::deprecate_warn(
    when = margot_positivity_deprecation_when,
    what = what,
    with = with,
    details = margot_positivity_deprecation_details()
  )
  invisible(NULL)
}

# the defunct error shared by the two retired enforcement exports. `what` names
# the export; `replacement` names what the caller should do instead.
margot_positivity_defunct <- function(what, replacement, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "{.fn {what}} is defunct.",
      "x" = paste(
        "It implemented the retired enforcement machinery: a mechanical pass/fail",
        "over threshold constants. The 29 July 2026 guide-architecture change",
        "removed every traffic light, tolerance and verdict from the workflow."
      ),
      "i" = replacement,
      "i" = "See the `margot.lmtp` question-review workflow for the replacement."
    ),
    class = "margot_error_defunct",
    call = call
  )
}

# errors when a caller supplies an argument removed with the enforcement
# machinery, so a stale call fails loudly rather than losing its threshold in
# silence. `dots` is the captured `...`; `removed` names the retired arguments.
margot_stop_removed_arguments <- function(dots, removed, what, call = rlang::caller_env()) {
  supplied <- names(dots) %||% character()
  hit <- intersect(removed, supplied)
  extra <- setdiff(supplied[nzchar(supplied)], removed)
  unnamed <- sum(!nzchar(supplied)) + max(0L, length(dots) - length(supplied))
  if (!length(hit) && !length(extra) && unnamed == 0L) {
    return(invisible(NULL))
  }
  bullets <- character()
  if (length(hit)) {
    bullets <- c(bullets, "x" = paste0(
      "Removed with the enforcement machinery: ",
      paste(paste0("`", hit, "`"), collapse = ", "), "."
    ))
  }
  if (length(extra) || unnamed > 0L) {
    labels <- if (length(extra)) paste(paste0("`", extra, "`"), collapse = ", ") else "<unnamed>"
    bullets <- c(bullets, "x" = paste0("Not an argument of `", what, "()`: ", labels, "."))
  }
  cli::cli_abort(
    c(
      paste0("`", what, "()` no longer accepts threshold or verdict arguments."),
      bullets,
      "i" = paste(
        "Descriptive summaries remain. Drop the retired arguments and read the",
        "quantities directly, or precommit an expectation with",
        "`margot.lmtp::margot_lmtp_expectations_spec()`."
      )
    ),
    class = "margot_error_removed_argument",
    call = call
  )
}

# TRUE when the sealed-workflow package is installed. kept as a named helper so
# the margot_lmtp() estimator-contract path is testable without margot.lmtp.
has_margot_lmtp <- function() {
  requireNamespace("margot.lmtp", quietly = TRUE)
}
