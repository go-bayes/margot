#' Causal Contrast Marginal Effects Estimation
#'
#' This function estimates the average treatment effect (ATE) or average treatment effect on the treated (ATT)
#' using generalized linear models (GLMs). It supports handling of continuous and categorical treatments, optional
#' use of spline transformations, and adjustments for multiple imputation datasets.
#'
#' @param df Data frame containing the data.
#' @param Y The response variable in the data frame.
#' @param X The treatment or exposure variable in the data frame.
#' @param baseline_vars A vector of names of baseline covariates to adjust for in the model.
#' @param treat_0 The reference level of the treatment variable, corresponding to no treatment or control condition.
#' @param treat_1 The active level of the treatment variable, corresponding to receiving the treatment.
#' @param estimand A character vector specifying the estimand; "ATE" for Average Treatment Effect or "ATT" for Average Treatment Effect on the Treated.
#' @param type A character vector specifying the type of effect size; "RD" for Risk Difference or "RR" for Risk Ratio.
#' @param nsims Number of simulations to perform, relevant when handling multiple imputation datasets.
#' @param cores Number of cores to use for parallel processing.
#' @param family The family of the GLM to be used (e.g., "gaussian" for linear models).
#' @param weights Logical indicating whether to use weights in the model fitting.
#' @param continuous_X Logical indicating whether the treatment variable X is continuous.
#' @param splines Logical indicating whether to use spline transformations for the treatment variable X.
#' @param vcov The method to use for variance-covariance matrix estimation.
#' @param verbose Logical indicating whether to display detailed output during model fitting.
#'
#' @return Depending on the 'type' specified, it returns a summary object containing either risk differences or risk ratios along with additional statistics like confidence intervals.
#'
#' @examples
#' # Assuming df is your dataset with variables 'outcome', 'treatment', 'age', and 'gender'
#' result <- causal_contrast_marginal(df = df, Y = "outcome", X = "treatment",
#'                                    baseline_vars = c("age", "gender"),
#'                                    treat_0 = "control", treat_1 = "exposed",
#'                                    estimand = "ATE", type = "RD", nsims = 100,
#'                                    cores = 2, family = "gaussian", weights = TRUE,
#'                                    continuous_X = FALSE, splines = FALSE,
#'                                    vcov = "HC3", verbose = TRUE)
#'
#' @importFrom stats glm
#' @importFrom parallel detectCores
#' @importFrom purrr map
#' @import glue
#' @import rlang
#' @export
causal_contrast_marginal <- function(df, Y, X, baseline_vars = baseline_vars, treat_0 = treat_0,
                                     treat_1 = treat_1, estimand = c("ATE", "ATT"), type = c("RR", "RD"),
                                     nsims = 200, cores = parallel::detectCores(), family = "gaussian",
                                     weights = TRUE, continuous_X = FALSE, splines = FALSE, vcov = "HC2",
                                     verbose = FALSE) {
  # Check if required packages are installed
  # required_packages <- c("clarify", "rlang", "glue", "parallel")
  # for (pkg in required_packages) {
  #   if (!requireNamespace(pkg, quietly = TRUE)) {
  #     stop(paste0("Package '", pkg, "' is needed for this function but is not installed"))
  #   }
  # }

  # check if the family argument is valid
  if (is.character(family)) {
    if (!family %in% c("gaussian", "binomial", "Gamma", "inverse.gaussian", "poisson", "quasibinomial", "quasipoisson", "quasi")) {
      stop("Invalid 'family' argument. Please specify a valid family function.")
    }
    family_fun <- get(family, mode = "function", envir = parent.frame())
  } else if (class(family) %in% c("family", "quasi")) {
    family_fun <- family
  } else {
    stop("Invalid 'family' argument. Please specify a valid family function or character string.")
  }

  if (continuous_X) {
    estimand <- "ATE"
    treat_0 <- as.numeric(treat_0)
    treat_1 <- as.numeric(treat_1)
    # warning("When continuous_X = TRUE, estimand is always set to 'ATE'")
  }


  # build formula string
  build_formula_str <- function(Y, X, continuous_X, splines, baseline_vars) {
    if (continuous_X && splines) {
      return(paste(Y, "~ bs(", X , ")", "*", "(", paste(baseline_vars, collapse = "+"), ")"))
    } else {
      return(paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")"))
    }
  }

  # fit models using the complete datasets (all imputations) or single dataset
  if ("wimids" %in% class(df)) {
    fits <- purrr::map(complete(df, "all"), function(d) {
      weight_var <- if (weights) d$weights else NULL
      formula_str <- build_formula_str(Y, X, continuous_X, splines, baseline_vars)
      glm(as.formula(formula_str), weights = weight_var, family = family_fun, data = d)
    })
    sim.imp <- misim(fits, n = nsims, vcov = vcov)
  } else {
    weight_var <- if (weights) df$weights else NULL
    formula_str <- build_formula_str(Y, X, continuous_X, splines, baseline_vars)
    fit <- glm(as.formula(formula_str), weights = weight_var, family = family_fun, data = df)
    sim.imp <- sim(fit, n = nsims, vcov = vcov)
  }

  if (continuous_X) {
    estimand <- "ATE"
    # warning("When continuous_X = TRUE, estimand is always set to 'ATE'")
  }

  # Fit models using the complete datasets (all imputations)
  fits <-  lapply(complete(df, "all"), function(d) {
    # Set weights variable based on the value of 'weights' argument
    weight_var <- if (weights) d$weights else NULL

    # check if continuous_X and splines are both TRUE
    if (continuous_X && splines) {
      require(splines) # splines package
      formula_str <- paste(Y, "~ bs(", X , ")", "*", "(", paste(baseline_vars, collapse = "+"), ")")
    } else {
      formula_str <- paste(Y, "~", X , "*", "(", paste(baseline_vars, collapse = "+"), ")")
    }

    glm(
      as.formula(formula_str),
      weights = if (!is.null(weight_var)) weight_var else NULL,
      family = family,
      data = d
    )
  })
  # A `clarify_misim` object

  sim.imp <- misim(fits, n = nsims, vcov = vcov)

  # compute the average marginal effects

  if (!continuous_X && estimand == "ATT") {
    # build dynamic expression for subsetting
    subset_expr <- rlang::expr(!!rlang::sym(X) == !!treat_1)

    sim_estimand <- sim_ame(sim.imp,
                            var = X,
                            subset = eval(subset_expr),
                            cl = cores,
                            verbose = FALSE)
  } else {
    sim_estimand <- sim_ame(sim.imp, var = X, cl = cores, verbose = FALSE)

    # convert treat_0 and treat_1 into strings that represent the column names
    treat_0_name <- paste0("`E[Y(", treat_0, ")]`")
    treat_1_name <- paste0("`E[Y(", treat_1, ")]`")

    if (type == "RR") {
      rr_expression_str <- glue::glue("{treat_1_name}/{treat_0_name}")
      rr_expression <- rlang::parse_expr(rr_expression_str)

      # create a new column RR in the sim_estimand object
      sim_estimand <- transform(sim_estimand, RR = eval(rr_expression))

      # create a summary of sim_estimand
      sim_estimand_summary <- summary(sim_estimand)

      return(sim_estimand_summary)

    } else if (type == "RD") {
      rd_expression_str <- glue::glue("{treat_1_name} - {treat_0_name}")
      rd_expression <- rlang::parse_expr(rd_expression_str)

      # Create a new column RD in the sim_estimand object
      sim_estimand <- transform(sim_estimand, RD = eval(rd_expression))

      # Create a summary of sim_estimand
      sim_estimand_summary <- summary(sim_estimand)

      return(sim_estimand_summary)

    } else {
      stop("Invalid type. Please choose 'RR' or 'RD'")
    }
  }
}
