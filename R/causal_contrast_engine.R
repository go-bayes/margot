#' Compute Causal Contrasts
#'
#' Estimates causal contrasts using generalized linear models for different types of treatment effects (ATE, ATT)
#' and outcomes (RR, RD). Supports handling multiply imputed datasets and allows flexibility in model specification.
#'
#' @param df Data frame or `mids` object containing the data.
#' @param Y Response variable name as a string.
#' @param X Treatment or exposure variable name as a string.
#' @param baseline_vars Vector of baseline covariate names.
#' @param treat_0 Reference level of the treatment variable.
#' @param treat_1 Treatment level of interest for comparison.
#' @param estimand Type of causal estimand ("ATE", "ATT"); defaults to both.
#' @param type Type of effect size ("RR" for Risk Ratio, "RD" for Risk Difference); defaults to both.
#' @param nsims Number of simulations for bootstrap; defaults to 200.
#' @param cores Number of cores for parallel processing; uses all available cores by default.
#' @param family Model family as a string or family object; defaults to "gaussian".
#' @param weights The name of the weights variable in the data frame, or NULL if no weights are to be used.
#' @param continuous_X Whether X is a continuous variable; defaults to FALSE.
#' @param splines Whether to apply spline transformation to X; defaults to FALSE.
#' @param vcov Type of variance-covariance matrix for standard error estimation; defaults to "HC2".
#' @param verbose Whether to print detailed output; defaults to FALSE.
#' @return Depending on the configuration, returns a summary object containing estimated causal contrasts, confidence intervals, and potentially other diagnostics.
#'
#' @importFrom rlang expr sym parse_expr
#' @importFrom glue glue
#' @importFrom parallel detectCores
#' @importFrom mice complete
#' @importFrom cli cli_abort cli_alert_info cli_alert_warning
#' @importFrom clarify misim sim sim_ame
#' @keywords internal
# compute causal contrasts using glm and clarify simulation
causal_contrast_engine <- function(df, Y, X, baseline_vars, treat_0, treat_1,
                                   estimand = c("ATE", "ATT"), type = c("RR", "RD"),
                                   nsims = 200, cores = parallel::detectCores(),
                                   family = "gaussian", weights = TRUE,
                                   continuous_X = FALSE, splines = FALSE,
                                   vcov = "HC2", verbose = FALSE) {
  # ensure valid estimand and type
  estimand <- match.arg(estimand)
  type <- match.arg(type)

  # helper function to build formula string
  build_formula_str <- function(Y, X, continuous_X, splines, baseline_vars) {
    # build basic formula string
    formula_str <- paste(Y, "~", X)
    if (length(baseline_vars) > 0) {
      formula_str <- paste(formula_str, "+", paste(baseline_vars, collapse = " + "))
    }
    return(formula_str)
  }

  # helper function to process a single imputed dataset
  process_imputation <- function(imputed_data, weights) {
    # build formula from inputs
    formula_str <- build_formula_str(Y, X, continuous_X, splines, baseline_vars)
    # apply glm model
    fit <- glm(
      as.formula(formula_str),
      weights = weights,
      family = get(family, mode = "function", envir = parent.frame()),
      data = imputed_data
    )
    return(fit)
  }

  # if input is a wimids object, extract mids and weights
  if (inherits(df, "wimids")) {
    mids_obj <- df$object
    weights_list <- lapply(df$models, function(m) m$weights)
    # process each imputed dataset
    fits <- lapply(1:mids_obj$m, function(i) {
      imputed_data <- mice::complete(mids_obj, i)
      process_imputation(imputed_data, weights_list[[i]])
    })
    # create misim object
    sim.imp <- clarify::misim(fits, n = nsims, vcov = vcov)
  } else {
    # process single dataset
    weight_var <- if (!is.null(weights) && weights %in% names(df)) df[[weights]] else NULL
    fit <- process_imputation(df, weight_var)
    sim.imp <- clarify::sim(fit, n = nsims, vcov = vcov)
  }

  if (continuous_X) {
    estimand <- "ATE"
    if (verbose) {
      cli::cli_alert_info("using ATE for continuous exposure variable")
    }
  }

  # compute average marginal effects
  if (!continuous_X && estimand == "ATT") {
    subset_expr <- rlang::expr(!!rlang::sym(X) == !!treat_1)
    sim_estimand <- clarify::sim_ame(
      sim.imp,
      var = X,
      subset = eval(subset_expr),
      cl = cores,
      verbose = FALSE
    )
  } else {
    sim_estimand <- clarify::sim_ame(
      sim.imp,
      var = X,
      cl = cores,
      verbose = FALSE
    )

    treat_0_name <- paste0("`E[Y(", treat_0, ")]`")
    treat_1_name <- paste0("`E[Y(", treat_1, ")]`")

    if (type == "RR") {
      rr_expression_str <- glue::glue("{treat_1_name}/{treat_0_name}")
      rr_expression <- rlang::parse_expr(rr_expression_str)
      sim_estimand <- transform(sim_estimand, RR = eval(rr_expression))
      return(summary(sim_estimand))
    } else if (type == "RD") {
      rd_expression_str <- glue::glue("{treat_1_name} - {treat_0_name}")
      rd_expression <- rlang::parse_expr(rd_expression_str)
      sim_estimand <- transform(sim_estimand, RD = eval(rd_expression))
      return(summary(sim_estimand))
    } else {
      cli::cli_abort("invalid type. please choose 'RR' or 'RD'")
    }
  }
}
