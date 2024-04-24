#' Double Robust Marginal Estimation and Tabulation
#'
#' This function provides a double robust approach for estimating causal effects. It first computes
#' marginal effects using the `causal_contrast_marginal` function, then processes and tabulates these
#' results using `tab_engine_marginal`. The function handles both continuous and categorical variables,
#' and allows specification of the type of effect measure for both causal estimation and tabulation.
#'
#' @param df A data frame containing the dataset for analysis.
#' @param Y The name of the outcome variable as a string.
#' @param X The name of the treatment or exposure variable as a string.
#' @param baseline_vars A vector of covariate names included in the model.
#' @param treat_0 The reference level of the treatment variable.
#' @param treat_1 The treatment level of the treatment variable.
#' @param nsims The number of simulations to run, used in bootstrap or Monte Carlo methods.
#' @param cores The number of processor cores to use for parallel computation.
#' @param family A description of the error distribution and link function to be used in the model.
#' @param weights Logical, indicating whether to use weighting in the model estimation.
#' @param continuous_X Logical, indicating whether the treatment variable X is continuous.
#' @param splines Logical, indicating whether to use spline functions for continuous variables.
#' @param vcov The method to use for variance-covariance estimation.
#' @param verbose Logical, indicating whether to print detailed output during computation.
#' @param delta The assumed smallest worthwhile effect, used for E-value calculations in tabulation.
#' @param sd The standard deviation of the effect estimate, used for E-value calculations.
#' @param new_name A new name to assign to the tabulated output, typically describing the variable or model.
#' @param estimand Specifies the target of the causal inference, such as "ATE" (Average Treatment Effect) or "ATT" (Average Treatment on the Treated).
#' @param type_causal The type of effect size (e.g., "RR" for Risk Ratio or "RD" for Risk Difference) to be computed in the causal analysis.
#' @param type_tab The type of effect size to be used in the tabulation of results.
#'
#' @return A list containing two elements: `causal_results` with the results from the causal analysis, and
#'         `tab_results` with the tabulated results including E-values and other statistics.
#'
#' @examples
#' # Assuming you have a dataset `df_ate` and necessary variables defined
#' results <- double_robust_marginal(
#'   df = df_ate,
#'   Y = "t2_kessler_latent_anxiety_z",
#'   X = "treatment_var",
#'   baseline_vars = c("age", "gender"),
#'   treat_1 = "intervention",
#'   treat_0 = "control",
#'   nsims = 200,
#'   cores = 4,
#'   family = "gaussian",
#'   weights = TRUE,
#'   continuous_X = FALSE,
#'   splines = FALSE,
#'   estimand = "ATE",
#'   type_causal = "RD",
#'   type_tab = "RD",
#'   vcov = "HC2",
#'   new_name = "Test Model Effect",
#'   delta = 1,
#'   sd = 1
#' )
#'
#' @export
#' @importFrom parallel detectCores
#' @importFrom stats glm
#' @importFrom dplyr filter mutate rename
#' @importFrom EValue evalues.OLS evalues.RR
double_robust_marginal <- function(df, Y, X, baseline_vars, treat_0, treat_1, nsims, cores, family,
                                   weights = TRUE, continuous_X = FALSE, splines = FALSE, vcov = "HC2",
                                   verbose = FALSE, delta = 1, sd = 1, new_name, estimand = c("ATE", "ATT"),
                                   type_causal = c("RR", "RD"), type_tab = "RD") {
  # type <- match.arg(type)  # This selects the first element of the vector as the default

  # First, run the causal_contrast_marginal function with its specific type argument
  causal_results <- causal_contrast_marginal(
    df = df,
    Y = Y,
    X = X,
    baseline_vars = baseline_vars,
    treat_0 = treat_0,
    treat_1 = treat_1,
    estimand = estimand,
    type = type_causal,
    nsims = nsims,
    cores = cores,
    family = family,
    weights = weights,
    continuous_X = continuous_X,
    splines = splines,
    vcov = vcov,
    verbose = verbose
  )

  # Then, feed the results into tab_engine_marginal with its specific type argument
  tab_results <- tab_engine_marginal(
    x = causal_results,
    new_name = new_name,
    delta = delta,
    sd = sd,
    type = type_tab,
    continuous_X = continuous_X
  )

  # Return both results in a list for further usage or examination
  return(list(causal_results = causal_results, tab_results = tab_results))
}

