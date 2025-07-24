#' Create Simple QINI Baseline Object
#'
#' Creates a simple baseline object that represents the expected gain under
#' random treatment allocation. This serves as a robust fallback when maq
#' fails to generate a baseline.
#'
#' @param mean_tau The average treatment effect (mean of tau_hat)
#' @param n_points Number of points for the gain curve (default 100)
#' @param n_units Total number of units (for metadata)
#' 
#' @return A list mimicking maq output structure with:
#'   - _path: list with gain, spend, and std.err
#'   - mean_tau: the average treatment effect
#'   - baseline_type: "simple"
#'   - class: "qini_simple_baseline"
#'   
#' @keywords internal
margot_qini_simple_baseline <- function(mean_tau, n_points = 100, n_units = NULL) {
  # generate evenly spaced points from 0 to 1
  spend <- seq(0, 1, length.out = n_points)
  
  # under random allocation, gain is proportional to spend
  # gain(B) = B * mean(tau)
  gain <- spend * mean_tau
  
  # for a simple baseline, std.err could be approximated or set to 0
  # we'll use 0 for now as this is deterministic
  std_err <- rep(0, n_points)
  
  # create object mimicking maq structure
  result <- list(
    "_path" = list(
      spend = spend,
      gain = gain,
      std.err = std_err,
      complete.path = TRUE
    ),
    mean_tau = mean_tau,
    n_units = n_units,
    baseline_type = "simple"
  )
  
  class(result) <- c("qini_simple_baseline", "list")
  return(result)
}

#' Compute Average Gain for Simple Baseline
#'
#' @param object A qini_simple_baseline object
#' @param spend The spend level (proportion treated)
#' @param ... Additional arguments (ignored)
#' 
#' @return A list with estimate and std.err
#' @export
#' @keywords internal
average_gain.qini_simple_baseline <- function(object, spend, ...) {
  # for simple baseline, average gain is just spend * mean_tau
  estimate <- spend * object$mean_tau
  std_err <- 0  # deterministic, no uncertainty
  
  return(list(
    estimate = estimate,
    std.err = std_err
  ))
}

#' Compute Integrated Difference for Simple Baseline
#'
#' When comparing against a simple baseline, this computes the integrated
#' difference between the comparison curve and the simple baseline.
#'
#' @param object A maq object (the comparison curve)  
#' @param baseline A qini_simple_baseline object
#' @param spend The spend level up to which to integrate
#' @param ... Additional arguments (ignored)
#' 
#' @return A list with estimate and std.err
#' @keywords internal
integrated_difference_simple <- function(object, baseline, spend, ...) {
  # we need to integrate the difference between the curves
  # for the simple baseline, we know the curve is linear: gain = spend * mean_tau
  
  # get the path from the comparison object
  if (!is.null(object[["_path"]])) {
    comp_spend <- object[["_path"]]$spend
    comp_gain <- object[["_path"]]$gain
    comp_std_err <- object[["_path"]]$std.err
    
    # find points up to the spend level
    idx <- comp_spend <= spend
    if (sum(idx) < 2) {
      return(list(estimate = 0, std.err = 0))
    }
    
    comp_spend <- comp_spend[idx]
    comp_gain <- comp_gain[idx]
    
    # ensure we include the exact spend point
    if (max(comp_spend) < spend) {
      # interpolate to get the value at spend
      last_gain <- comp_gain[length(comp_gain)]
      comp_spend <- c(comp_spend, spend)
      comp_gain <- c(comp_gain, last_gain)  # extend last value
    }
    
    # compute baseline gains at the same spend points
    baseline_gain <- comp_spend * baseline$mean_tau
    
    # compute the integrated difference using trapezoidal rule
    n <- length(comp_spend)
    if (n < 2) {
      return(list(estimate = 0, std.err = 0))
    }
    
    # differences at each point
    diffs <- comp_gain - baseline_gain
    
    # trapezoidal integration
    widths <- diff(comp_spend)
    heights <- (diffs[-n] + diffs[-1]) / 2
    integrated_diff <- sum(widths * heights)
    
    # for std.err, we'd need to propagate uncertainty
    # for now, return 0 as baseline has no uncertainty
    std_err <- 0
    
    return(list(
      estimate = integrated_diff,
      std.err = std_err
    ))
  }
  
  return(list(estimate = 0, std.err = 0))
}