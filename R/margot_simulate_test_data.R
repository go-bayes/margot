#' Simulate Test Data for margot Development Functions
#'
#' Creates a consistent synthetic dataset for testing the development versions
#' of margot functions. Generates panel data with known treatment effects to
#' enable validation of causal inference methods.
#'
#' @param n Integer. Number of individuals (default: 5000)
#' @param k Integer. Number of time points including baseline (default: 3)
#' @param p_covars Integer. Number of baseline covariates (default: 20)
#' @param treatment_effects Named list specifying treatment effect patterns for
#'   each outcome. Options: "positive", "negative", "null", "heterogeneous".
#'   Default creates 4 outcomes with different patterns.
#' @param missing_prop Numeric. Proportion of missing data to introduce (default: 0.1)
#' @param binary_outcomes Logical vector indicating which outcomes should be binary
#'   (default: c(FALSE, FALSE, FALSE, TRUE))
#' @param censoring_rate Numeric. Rate of censoring/attrition (default: 0.1)
#' @param seed Integer. Random seed for reproducibility (default: 2025)
#'
#' @return A list containing:
#'   \item{data}{Data frame with all variables in wide format}
#'   \item{true_effects}{List of true treatment effect functions}
#'   \item{metadata}{List with generation parameters and variable information}
#'
#' @details
#' The function creates:
#' - Baseline covariates (B1-Bp) from multivariate normal
#' - Binary treatment (A) with propensity depending on covariates
#' - Multiple outcomes (Y1-Y4) with different treatment effect patterns:
#'   - Y1: Positive homogeneous effect (ATE = 0.3)
#'   - Y2: Negative homogeneous effect (ATE = -0.2)
#'   - Y3: Null effect (ATE = 0)
#'   - Y4: Strong heterogeneous effect based on B1 and B2
#'
#' Treatment assignment depends on B1 and B2 to create confounding.
#' Missing data is introduced randomly (MCAR) for testing robustness.
#'
#' @examples
#' # Generate default test dataset
#' test_data <- margot_simulate_test_data()
#' 
#' # Generate with more observations and covariates
#' test_data_large <- margot_simulate_test_data(n = 10000, p_covars = 30)
#' 
#' # Generate with custom treatment effects
#' test_data_custom <- margot_simulate_test_data(
#'   treatment_effects = list(
#'     Y1 = "heterogeneous",
#'     Y2 = "positive",
#'     Y3 = "heterogeneous"
#'   )
#' )
#'
#' @export
#' @keywords internal
#' @importFrom stats rnorm rbinom plogis
#' @importFrom MASS mvrnorm
margot_simulate_test_data <- function(
    n = 5000,
    k = 3,
    p_covars = 20,
    treatment_effects = list(
      Y1 = "positive",
      Y2 = "negative", 
      Y3 = "null",
      Y4 = "heterogeneous"
    ),
    missing_prop = 0.1,
    binary_outcomes = c(FALSE, FALSE, FALSE, TRUE),
    censoring_rate = 0.1,
    seed = 2025
) {
  
  # set seed for reproducibility
  set.seed(seed)
  
  # parameter validation
  if (n < 100) stop("n must be at least 100 for meaningful testing")
  if (k < 3) stop("k must be at least 3 for causal inference")
  if (p_covars < 5) stop("p_covars must be at least 5")
  if (missing_prop < 0 || missing_prop > 0.5) {
    stop("missing_prop must be between 0 and 0.5")
  }
  
  # generate baseline covariates with correlation structure
  Sigma <- matrix(0.3, p_covars, p_covars)
  diag(Sigma) <- 1
  B <- MASS::mvrnorm(n, mu = rep(0, p_covars), Sigma = Sigma)
  colnames(B) <- paste0("B", 1:p_covars)
  
  # create data frame
  df <- data.frame(id = 1:n, B)
  
  # define true propensity score function
  # depends on B1 and B2 to create confounding
  prop_score <- plogis(-0.5 + 0.5 * df$B1 + 0.3 * df$B2)
  
  # generate treatment assignment
  df$A <- rbinom(n, 1, prop_score)
  
  # define true treatment effect functions
  true_tau <- list()
  
  # generate outcomes based on treatment_effects specification
  outcome_names <- names(treatment_effects)
  n_outcomes <- length(outcome_names)
  
  # extend binary_outcomes if needed
  if (length(binary_outcomes) < n_outcomes) {
    binary_outcomes <- c(binary_outcomes, 
                        rep(FALSE, n_outcomes - length(binary_outcomes)))
  }
  
  for (i in seq_along(outcome_names)) {
    outcome <- outcome_names[i]
    effect_type <- treatment_effects[[outcome]]
    
    # baseline outcome model (depends on B1, B3, B5)
    mu0 <- 0.2 * df$B1 + 0.15 * df$B3 - 0.1 * df$B5
    
    # treatment effect based on type
    if (effect_type == "positive") {
      tau <- rep(0.3, n)
      true_tau[[outcome]] <- function(x) 0.3
    } else if (effect_type == "negative") {
      tau <- rep(-0.2, n) 
      true_tau[[outcome]] <- function(x) -0.2
    } else if (effect_type == "null") {
      tau <- rep(0, n)
      true_tau[[outcome]] <- function(x) 0
    } else if (effect_type == "heterogeneous") {
      # strong heterogeneous effect based on B1 and B2
      tau <- 0.5 * df$B1 - 0.4 * df$B2 + 0.2 * df$B1 * df$B2
      true_tau[[outcome]] <- function(x) {
        0.5 * x[,"B1"] - 0.4 * x[,"B2"] + 0.2 * x[,"B1"] * x[,"B2"]
      }
    } else {
      stop("Unknown effect_type: ", effect_type)
    }
    
    # generate outcome
    Y <- mu0 + tau * df$A + rnorm(n, 0, 0.5)
    
    # convert to binary if specified
    if (binary_outcomes[i]) {
      Y <- as.integer(plogis(Y) > runif(n))
    }
    
    df[[outcome]] <- Y
  }
  
  # add censoring indicator based on treatment and covariates
  # higher probability of censoring for treated units with high B1
  censor_prob <- plogis(-2.5 + 0.3 * df$A + 0.2 * df$B1)
  censor_prob <- pmax(pmin(censor_prob, censoring_rate * 2), censoring_rate / 2)
  df$censored <- rbinom(n, 1, censor_prob)
  
  # introduce missing data (MCAR)
  if (missing_prop > 0) {
    # don't make id, treatment, outcomes, or censoring indicator missing
    # only introduce missing data in covariates (B variables)
    covariate_vars <- grep("^B[0-9]+$", names(df), value = TRUE)
    
    for (var in covariate_vars) {
      missing_idx <- sample(n, floor(n * missing_prop))
      df[missing_idx, var] <- NA
    }
  }
  
  # create metadata
  metadata <- list(
    n = n,
    k = k,
    p_covars = p_covars,
    outcome_names = outcome_names,
    treatment_effects = treatment_effects,
    binary_outcomes = binary_outcomes,
    missing_prop = missing_prop,
    censoring_rate = censoring_rate,
    seed = seed,
    prop_score_formula = "plogis(-0.5 + 0.5 * B1 + 0.3 * B2)",
    baseline_formula = "0.2 * B1 + 0.15 * B3 - 0.1 * B5",
    covariate_names = paste0("B", 1:p_covars),
    treatment_name = "A",
    censoring_name = "censored"
  )
  
  # return list
  list(
    data = df,
    true_effects = true_tau,
    metadata = metadata
  )
}

#' Generate Test Data with Flipped Outcomes
#'
#' Convenience function that generates test data and creates flipped versions
#' of specified outcomes for testing flip forest functionality.
#'
#' @param flip_outcomes Character vector of outcome names to flip (default: "Y1")
#' @param ... Additional arguments passed to margot_simulate_test_data()
#'
#' @return List with same structure as margot_simulate_test_data() plus
#'   flipped outcome columns (with "_r" suffix)
#'
#' @examples
#' # Generate test data with Y1 flipped
#' test_data_flip <- margot_simulate_test_data_flip()
#' 
#' # Generate test data with Y1 and Y4 flipped
#' test_data_flip_multi <- margot_simulate_test_data_flip(
#'   flip_outcomes = c("Y1", "Y4")
#' )
#'
#' @export
margot_simulate_test_data_flip <- function(
    flip_outcomes = "Y1",
    ...
) {
  # generate base data
  test_data <- margot_simulate_test_data(...)
  
  # flip specified outcomes
  for (outcome in flip_outcomes) {
    if (outcome %in% names(test_data$data)) {
      # create flipped version
      col_name_r <- paste0(outcome, "_r")
      
      # get outcome range
      y_vals <- test_data$data[[outcome]]
      y_vals_clean <- y_vals[!is.na(y_vals)]
      
      if (length(unique(y_vals_clean)) == 2) {
        # binary outcome - simple flip
        test_data$data[[col_name_r]] <- 1 - test_data$data[[outcome]]
      } else {
        # continuous outcome - flip around midpoint
        y_min <- min(y_vals_clean)
        y_max <- max(y_vals_clean)
        y_mid <- (y_min + y_max) / 2
        test_data$data[[col_name_r]] <- 2 * y_mid - test_data$data[[outcome]]
      }
      
      # update metadata
      test_data$metadata$flipped_outcomes <- c(
        test_data$metadata$flipped_outcomes,
        outcome
      )
    }
  }
  
  test_data
}