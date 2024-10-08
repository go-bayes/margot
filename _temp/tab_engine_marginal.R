#' Tabulate Marginal Effects with E-Values
#'
#' This function processes simulation results to tabulate marginal effects along with E-values,
#' providing a summary suited for reporting. It supports both risk difference (RD) and risk ratio (RR)
#' types of estimates and handles continuous and categorical treatment variables.
#'
#' @param x A data frame or matrix containing simulation results to be processed.
#' @param new_name A new name to assign to the output row, typically describing the variable or model.
#' @param delta The assumed smallest worthwhile effect, used for E-value calculations.
#' @param sd The standard deviation of the effect estimate, used for E-value calculations.
#' @param type Character vector specifying the scale of effect size, either "RD" or "RR".
#'        This parameter determines how the effects are calculated and presented.
#' @param continuous_X Logical indicating whether the treatment variable X is continuous.
#'        If TRUE, adjusts row names based on the type parameter.
#'
#' @return A data frame with the specified new_name as a row name. The data frame includes
#'         effect estimates, confidence intervals, E-values, and other relevant statistics formatted
#'         for easy reporting.
#'
#' @examples
#' # Assuming you have results from a simulation or model in `results_df`
#' tabulated_results <- tab_engine_marginal(x = results_df,
#'                                          new_name = "Treatment Effect",
#'                                          delta = 1,
#'                                          sd = 0.2,
#'                                          type = "RD")  # Corrected 'scale' to 'type'
#'
#' @importFrom dplyr filter mutate rename select
#' @importFrom EValue evalues.OLS evalues.RR
#' @internal
tab_engine_marginal <- function(x, new_name, delta = 1, sd = 1, type = c("RD", "RR"), continuous_X = FALSE) {
  require("EValue")
  require(dplyr)

  type <- match.arg(type, choices = c("RD", "RR"))
  x <- as.data.frame(x)

  if (continuous_X) {
    rownames(x) <- type
  }

  out <- x %>%
    dplyr::filter(row.names(x) == type) %>%
    dplyr::mutate(across(where(is.numeric), round, digits = 4))

  if (type == "RD") {
    out <- out %>%
      dplyr::rename("E[Y(1)]-E[Y(0)]" = Estimate)
  } else {
    out <- out %>%
      dplyr::rename("E[Y(1)]/E[Y(0)]" = Estimate)
  }

  if (!"outcome" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(outcome = new_name)
  } else {
    out$outcome <- new_name  # Just update the existing 'outcome' column instead of adding a new one
  }

  out <- dplyr::select(out, outcome, everything())

  if (type == "RD") {
    out <- out %>%
      dplyr::mutate(standard_error = abs(`2.5 %` - `97.5 %`) / 3.92)
    evalout <- as.data.frame(round(EValue::evalues.OLS(out[1, "E[Y(1)]-E[Y(0)]"], se = out$standard_error, sd = sd, delta = delta, true = 0), 3))
  } else {
    evalout <- as.data.frame(round(EValue::evalues.RR(out[1, "E[Y(1)]/E[Y(0)]"], lo = out[1, "2.5 %"], hi = out[1, "97.5 %"], true = 1), 3))
  }

  evalout2 <- subset(evalout[2, ])
  evalout3 <- evalout2 %>% dplyr::select_if(~ !any(is.na(.)))
  colnames(evalout3) <- c("E_Value", "E_Val_bound")

  out <- cbind.data.frame(out, evalout3)
  return(out)
}
# tab_engine_marginal <- function(x,
#                                 new_name,
#                                 delta = 1,
#                                 sd = 1,
#                                 type = c("RD", "RR"),
#                                 continuous_X = FALSE) {
#   require("EValue")
#   require(dplyr)
#
#   # Match the argument to ensure it's valid and set the default
#   type <- match.arg(type, choices = c("RD", "RR"))
#
#   x <- as.data.frame(x)
#
#   # Adjust rownames if continuous_X is true
#   if (continuous_X) {
#     rownames(x) <- type
#   }
#
#   # Process data based on type
#   out <- x %>%
#     dplyr::filter(row.names(x) == type) %>%
#     dplyr::mutate(across(where(is.numeric), round, digits = 4))
#
#   # Rename based on the type
#   if (type == "RD") {
#     out <- out %>%
#       dplyr::rename("E[Y(1)]-E[Y(0)]" = Estimate)
#   } else {
#     out <- out %>%
#       dplyr::rename("E[Y(1)]/E[Y(0)]" = Estimate)
#   }
#
#   # Set the new name for the outcome
#   out <- out %>%
#     dplyr::mutate(outcome = new_name) %>%
#     dplyr::select(outcome, everything())  # This moves 'outcome' to the first position
#
#   # Calculate E-values based on the type
#   if (type == "RD") {
#     out <- out %>%
#       dplyr::mutate(standard_error = abs(`2.5 %` - `97.5 %`) / 3.92)
#     evalout <- as.data.frame(round(
#       EValue::evalues.OLS(
#         out[1, "E[Y(1)]-E[Y(0)]"],
#         se = out$standard_error,
#         sd = sd,
#         delta = delta,
#         true = 0
#       ), 3))
#   } else {
#     evalout <- as.data.frame(round(EValue::evalues.RR(
#       out[1, "E[Y(1)]/E[Y(0)]"],
#       lo = out[1, "2.5 %"],
#       hi = out[1, "97.5 %"],
#       true = 1
#     ), 3))
#   }
#
#   evalout2 <- subset(evalout[2, ])
#   evalout3 <- evalout2 %>%
#     dplyr::select_if(~ !any(is.na(.)))
#   colnames(evalout3) <- c("E_Value", "E_Val_bound")
#
#   # Combine results with E-values
#   out <- cbind.data.frame(out, evalout3)
#   return(out)
# }

