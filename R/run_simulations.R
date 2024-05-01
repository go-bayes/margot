#' Run Simulations for Estimating ATE
#'
#' This function simulates data and estimates the Average Treatment Effect (ATE)
#' using different methods under a specified causal model.
#'
#' @param num_simulations Integer, number of simulations to run.
#' @param N Integer, sample size for each simulation.
#' @param prob_L1 Double, probability parameter for generating L1.
#' @param A_on_Y Double, effect size of A on Y.
#' @param L_on_A Double, effect size of L on A.
#' @param L_on_Y Double, effect size of L on Y.
#' @param method Character, method for IPTW and DR adjustment ("ps" or other).
#' @return A tibble containing the estimated ATEs and their confidence intervals for each method.
#'
#' @export
run_simulations <- function(num_simulations, N, prob_L1, A_on_Y, L_on_A, L_on_Y, method = "ps") {
  if (!is.numeric(num_simulations) || num_simulations < 1)
    stop("num_simulations must be a positive integer.")
  if (!is.numeric(N) || N < 1)
    stop("N must be a positive integer.")

  # Initialise the matrix to store results
  simulations <- matrix(ncol = 4, nrow = num_simulations)
  colnames(simulations) <- c("ATE_unadjusted", "ATE_iptw", "ATE_gcomp", "ATE_dr")

  for (i in seq_len(num_simulations)) {
    # Generate synthetic data based on specified causal parameters
    data <- generate_data(N, prob_L1, A_on_Y, L_on_A, L_on_Y)

    # Estimate ATE using various methods
    simulations[i, 1] <- with(data, mean(Y[A == 1]) - mean(Y[A == 0]))
    simulations[i, 2] <- estimate_ATE_iptw(data, method)
    simulations[i, 3] <- estimate_ATE_gcomp(data)
    simulations[i, 4] <- estimate_ATE_dr(data, method)
  }

  # Compute mean ATE and confidence intervals
  mean_ATE <- colMeans(simulations, na.rm = TRUE)
  CI <- apply(simulations, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))

  # Compile results into a tibble
  results <- tibble::tibble(
    estimator = colnames(simulations),
    mean_ATE = mean_ATE,
    CI_lower = CI[1, ],
    CI_upper = CI[2, ]
  )

  return(results)
}
