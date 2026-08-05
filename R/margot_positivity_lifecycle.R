# lifecycle helpers for positivity enforcement removed on 29 July 2026. Margot
# retains descriptive density-ratio reports but derives no identification
# judgement from a numerical threshold.

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
      "i" = replacement
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
        "quantities directly against the expectations in the study protocol."
      )
    ),
    class = "margot_error_removed_argument",
    call = call
  )
}
