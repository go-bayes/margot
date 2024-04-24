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
#' @param weights Whether to use sample weights; defaults to TRUE.
#' @param continuous_X Whether X is a continuous variable; defaults to FALSE.
#' @param splines Whether to apply spline transformation to X; defaults to FALSE.
#' @param vcov Type of variance-covariance matrix for standard error estimation; defaults to "HC2".
#' @param verbose Whether to print detailed output; defaults to FALSE.
#' @return Depending on the configuration, returns a summary object containing estimated causal contrasts, confidence intervals, and potentially other diagnostics.
#' @export
#' @importFrom stats glm
#' @importFrom rlang expr sym
#' @importFrom glue glue
#' @importFrom parallel detectCores
causal_contrast_engine <- function(df,
                                   Y,
                                   X,
                                   baseline_vars = baseline_vars,
                                   treat_0 = treat_0,
                                   treat_1 = treat_1,
                                   estimand = c("ATE", "ATT"),
                                   type = c("RR", "RD"),
                                   nsims = 200,
                                   cores = parallel::detectCores(),
                                   family = "gaussian",
                                   weights = TRUE,
                                   continuous_X = FALSE,
                                   splines = FALSE,
                                   vcov = "HC2",
                                   verbose = FALSE) {
  # Check if required packages are installed
  required_packages <- c("clarify", "rlang", "glue", "parallel")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0(
        "Package '",
        pkg,
        "' is needed for this function but is not installed"
      ))
    }
  }


  # Prepare the progress bar
  pb <- progress::progress_bar$new(
    format = "  fitting models [:bar] :percent :elapsed/:eta",
    total = length(df), clear = FALSE, width = 60
  )



  # check if the family argument is valid
  if (is.character(family)) {
    if (!family %in% c(
      "gaussian",
      "binomial",
      "Gamma",
      "inverse.gaussian",
      "poisson",
      "quasibinomial",
      "quasipoisson",
      "quasi"
    )) {
      stop("Invalid 'family' argument. Please specify a valid family function.")
    }
    family_fun <-
      get(family, mode = "function", envir = parent.frame())
  } else if (class(family) %in% c("family", "quasi")) {
    family_fun <- family
  } else {
    stop(
      "Invalid 'family' argument. Please specify a valid family function or character string."
    )
  }

  # build formula string
  build_formula_str <-
    function(Y,
             X,
             continuous_X,
             splines,
             baseline_vars) {
      if (continuous_X && splines) {
        return(paste(
          Y,
          "~ bs(",
          X ,
          ")",
          "*",
          "(",
          paste(baseline_vars, collapse = "+"),
          ")"
        ))
      } else {
        return(paste(
          Y,
          "~",
          X ,
          "*",
          "(",
          paste(baseline_vars, collapse = "+"),
          ")"
        ))
      }
    }

  # fit models using the complete datasets (all imputations) or single dataset
  if ("wimids" %in% class(df)) {
    fits <- purrr::map(complete(df, "all"), function(d) {
      weight_var <- if (weights)
        d$weights
      else
        NULL
      formula_str <-
        build_formula_str(Y, X, continuous_X, splines, baseline_vars)
      glm(
        as.formula(formula_str),
        weights = weight_var,
        family = family_fun,
        data = d
      )
    })
    sim.imp <- misim(fits, n = nsims, vcov = vcov)
  } else {
    weight_var <- if (weights)
      df$weights
    else
      NULL
    formula_str <-
      build_formula_str(Y, X, continuous_X, splines, baseline_vars)
    fit <-
      glm(
        as.formula(formula_str),
        weights = weight_var,
        family = family_fun,
        data = df
      )
    sim.imp <- sim(fit, n = nsims, vcov = vcov)
  }

  if (continuous_X) {
    estimand <- "ATE"
    # warning("When continuous_X = TRUE, estimand is always set to 'ATE'")
  }

  # Fit models using the complete datasets (all imputations)
  fits <-  lapply(complete(df, "all"), function(d) {
    # Set weights variable based on the value of 'weights' argument
    weight_var <- if (weights)
      d$weights
    else
      NULL

    # check if continuous_X and splines are both TRUE
    if (continuous_X && splines) {
      require(splines) # splines package
      formula_str <-
        paste(Y,
              "~ bs(",
              X ,
              ")",
              "*",
              "(",
              paste(baseline_vars, collapse = "+"),
              ")")
    } else {
      formula_str <-
        paste(Y,
              "~",
              X ,
              "*",
              "(",
              paste(baseline_vars, collapse = "+"),
              ")")
    }

    glm(
      as.formula(formula_str),
      weights = if (!is.null(weight_var))
        weight_var
      else
        NULL,
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

    sim_estimand <- sim_ame(
      sim.imp,
      var = X,
      subset = eval(subset_expr),
      cl = cores,
      verbose = FALSE
    )
  } else {
    sim_estimand <-
      sim_ame(sim.imp,
              var = X,
              cl = cores,
              verbose = FALSE)

    # convert treat_0 and treat_1 into strings that represent the column names
    treat_0_name <- paste0("`E[Y(", treat_0, ")]`")
    treat_1_name <- paste0("`E[Y(", treat_1, ")]`")

    if (type == "RR") {
      rr_expression_str <- glue::glue("{treat_1_name}/{treat_0_name}")
      rr_expression <- rlang::parse_expr(rr_expression_str)

      # create a new column RR in the sim_estimand object
      sim_estimand <-
        transform(sim_estimand, RR = eval(rr_expression))

      # create a summary of sim_estimand
      sim_estimand_summary <- summary(sim_estimand)

      return(sim_estimand_summary)

    } else if (type == "RD") {
      rd_expression_str <- glue::glue("{treat_1_name} - {treat_0_name}")
      rd_expression <- rlang::parse_expr(rd_expression_str)

      # Create a new column RD in the sim_estimand object
      sim_estimand <-
        transform(sim_estimand, RD = eval(rd_expression))

      # Create a summary of sim_estimand
      sim_estimand_summary <- summary(sim_estimand)

      return(sim_estimand_summary)

    } else {
      stop("Invalid type. Please choose 'RR' or 'RD'")
    }
  }
}


