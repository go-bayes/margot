#' Compute Difference in Gains and Integrated Difference Between Reference and Comparison Curves
#'
#' This function computes the difference in average gains and the integrated difference
#' between a reference curve (maq object) and a comparison curve at a specified spend level.
#' It returns a list of formatted strings for easy use in Quarto markdown text.
#'
#' @param mc_result A list containing the results from margot_multi_arm_causal_forest().
#' @param outcome_var A character string specifying the name of the outcome variable.
#' @param reference_curve A character string specifying the name of the reference Qini curve (e.g., "baseline").
#' @param comparison_curve A character string specifying the name of the comparison Qini curve (e.g., "arm2").
#' @param spend A numeric value specifying the spend level (between 0 and 1).
#' @param digits An integer specifying the number of decimal places to round the output. Default is 3.
#'
#' @return A list containing formatted strings for use in Quarto markdown text:
#'   \item{diff_gain}{Formatted string for difference in gains}
#'   \item{int_diff}{Formatted string for integrated difference}
#'   \item{summary}{A summary sentence of the comparison}
#'
#' @examples
#' \dontrun{
#' # Assuming mc_result is the result of margot_multi_arm_causal_forest()
#' result <- margot_summary_cate_difference_gain(mc_result,
#'                                  outcome_var = "model_Y",
#'                                  reference_curve = "baseline",
#'                                  comparison_curve = "arm2",
#'                                  spend = 0.3)
#' # Use in text
#' glue::glue("The difference in gains is {result$diff_gain}. {result$summary}")
#' }
#'
#' @import glue
#' @import maq
#'
#' @export
margot_summary_cate_difference_gain <- function(mc_result, outcome_var, reference_curve, comparison_curve, spend, digits = 3) {
  # Check that outcome_var is in mc_result$results
  if (!outcome_var %in% names(mc_result$results)) {
    stop("Outcome variable not found in mc_result$results: ", outcome_var)
  }

  # Get the qini_objects
  qini_objects <- mc_result$results[[outcome_var]]$qini_objects

  # Check that qini_objects is not NULL
  if (is.null(qini_objects)) {
    stop("Qini objects not found for the specified outcome variable: ", outcome_var)
  }

  # Check that the specified curves are in qini_objects
  if (!reference_curve %in% names(qini_objects)) {
    stop("Reference curve not found in qini_objects: ", reference_curve)
  }
  if (!comparison_curve %in% names(qini_objects)) {
    stop("Comparison curve not found in qini_objects: ", comparison_curve)
  }

  # Get the maq objects for the specified curves
  maq_object_ref <- qini_objects[[reference_curve]]
  maq_object_comp <- qini_objects[[comparison_curve]]

  # Check that maq_objects are not NULL
  if (is.null(maq_object_ref)) {
    stop("maq object is NULL for reference curve: ", reference_curve)
  }
  if (is.null(maq_object_comp)) {
    stop("maq object is NULL for comparison curve: ", comparison_curve)
  }

  # Compute the average gains for both curves
  avg_gain_ref <- maq::average_gain(maq_object_ref, spend = spend)
  avg_gain_comp <- maq::average_gain(maq_object_comp, spend = spend)

  # Check if the result is an atomic vector and convert to list if necessary
  if (is.atomic(avg_gain_ref)) {
    avg_gain_ref <- list(estimate = avg_gain_ref[1], std.err = avg_gain_ref[2])
  }
  if (is.atomic(avg_gain_comp)) {
    avg_gain_comp <- list(estimate = avg_gain_comp[1], std.err = avg_gain_comp[2])
  }

  # Compute the difference in gains (comparison - reference)
  diff_estimate <- avg_gain_comp$estimate - avg_gain_ref$estimate
  diff_std_err <- sqrt(avg_gain_ref$std.err^2 + avg_gain_comp$std.err^2)

  # Round the results to the specified number of decimal places
  diff_estimate <- round(diff_estimate, digits)
  diff_std_err <- round(diff_std_err, digits)

  # Compute the integrated difference between the two curves
  integrated_diff <- maq::integrated_difference(maq_object_comp, maq_object_ref, spend = spend)

  # Check if the integrated difference result is an atomic vector
  if (is.atomic(integrated_diff)) {
    integrated_diff <- list(estimate = integrated_diff[1], std.err = integrated_diff[2])
  }

  # Round the integrated difference results to exactly 3 decimal places
  integrated_diff$estimate <- round(integrated_diff$estimate, 3)
  integrated_diff$std.err <- round(integrated_diff$std.err, 3)

  # Format the results using glue
  diff_gain <- glue::glue("{diff_estimate} (SE: {diff_std_err})")
  int_diff <- glue::glue("{format(integrated_diff$estimate, nsmall = 3)} (SE: {format(integrated_diff$std.err, nsmall = 3)})")

  summary <- glue::glue("Using {reference_curve} as the reference condition, {comparison_curve} shows a difference in gains of {diff_gain} ",
                        "at a {spend*100}% spend level. The integrated difference between {comparison_curve} and {reference_curve} is {int_diff}.")

  # Return a list of formatted strings
  return(list(
    diff_gain = diff_gain,
    int_diff = int_diff,
    summary = summary
  ))
}
