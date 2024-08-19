#' Generate Statistical Estimator Description
#'
#' @param estimators Character vector specifying the statistical estimators used.
#'
#' @return A character string containing the statistical estimator description in markdown format.
#'
#' @export
boilerplate_methods_statistical_estimator <- function(estimators = "lmtp") {
  if (is.character(estimators) && length(estimators) == 1) {
    estimators <- list(estimators)
  }

  estimator_texts <- list(
    lmtp = "
### Longitudinal Modified Treatment Policy (LMTP) Estimator

We perform statistical estimation using a semi-parametric Targeted Minimum Loss-based Estimation (TMLE) approach, specifically the Longitudinal Modified Treatment Policy (LMTP) estimator. TMLE is a robust method for estimating causal effects while providing valid statistical uncertainty measures [@van2014targeted; @van2012targeted].

TMLE operates through a two-step process:

1. **Initial Modeling:** TMLE models the relationship between treatments, covariates, and outcomes. We employ machine learning algorithms for this step, allowing us to flexibly model complex, high-dimensional covariate spaces without imposing restrictive model assumptions [@van2014discussion; @vanderlaan2011; @vanderlaan2018]. This results in a set of initial estimates for these relationships.

2. **Targeting:** TMLE 'targets' these initial estimates by incorporating information about the observed data distribution to improve the accuracy of the causal effect estimate. This iterative updating process adjusts the initial estimates towards the true causal effect, guided by the efficient influence function.

TMLE offers several advantages for causal inference:

1. Double robust estimation: the estimator is consistent if either the outcome model or the treatment mechanism model is correctly specified (but not necessarily both);
2. Efficiency in the presence of time-varying confounding;
3. Ability to handle complex longitudinal data structures and time-varying treatments;
4. Flexibility in specifying dynamic treatment regimes;
5. Compatibility with machine learning methods for initial estimation steps;
6. Handling of missing data from attrition, loss-to-follow up using inverse-probability of censoring weighting.

We use cross-validation to avoid overfitting, following pre-stated protocols [@bulbulia2024PRACTICAL]. The integration of TMLE and machine learning reduces dependence on restrictive modeling assumptions and introduces an additional layer of robustness.

We perform estimation using the `lmtp` package [@williams2021]. We use the `superlearner` library for semi-parametric estimation with the predefined libraries `SL.ranger`, `SL.glmnet`, and `SL.xgboost` [@polley2023; @xgboost2023; @Ranger2017]. For further details on targeted learning using the `lmtp` package, see [@hoffman2022; @hoffman2023; @díaz2021]. Graphs, tables, and output reports are created using the `margot` package [@margot2024].
    ",
    sdr = "
### Sequentially Doubly Robust (SDR) Estimator

We employ a semi-parametric estimator known as Sequentially Doubly Robust (SDR) estimation, which can estimate the causal effect of modified treatment policies on outcomes over time [@díaz2021]. This estimator belongs to the broader class of doubly-robust targeted learning frameworks developed by [@vanderlaan2011; @vanderlaan2018].

SDR operates through the following process:

1. **Initial Modeling:** It employs machine learning algorithms to flexibly model the relationship between treatments, covariates, and outcomes at each time point. This flexibility allows SDR to account for complex, high-dimensional covariate spaces without imposing restrictive parametric modeling assumptions.

2. **Sequential Estimation:** SDR applies a sequential modelling approach, estimating the treatment effect by working backwards in time. At each time point, SDR combines the outcome regression and the propensity score to construct an unbiased estimating equation. This process naturally incorporates the time-dependent nature of the data and interventions.

SDR offers several advantages for causal inference:

1. Sequential double robustness: The estimator is consistent if, for each time point, either the outcome model or the treatment mechanism model is correctly specified (but not necessarily both);
2. Efficiency in the presence of time-varying confounding;
3. Ability to handle complex longitudinal data structures and time-varying treatments;
4. Flexibility in specifying dynamic treatment regimes;
5. Compatibility with machine learning methods for initial estimation steps;
6. Handling of missing data from attrition, loss-to-follow up using inverse-probability of censoring weighting.

SDR uses cross-validation to avoid over-fitting and ensure that the estimator performs well in finite samples. The combination of SDR and machine learning technologies reduces the dependence on restrictive modeling assumptions and introduces an additional layer of robustness.

Estimation is performed using the `lmtp` package [@williams2021; @díaz2021; @hoffman2023]. For further details on the theoretical properties and practical applications of SDR, see [@hoffman2022; @hoffman2023; @díaz2021]. We employ a semi-parametric estimator known as Sequentially Doubly Robust (SDR) estimation, which can estimate the causal effect of modified treatment policies on outcomes over time [@díaz2021]. This estimator belongs to the broader class of doubly-robust targeted learning frameworks developed by [@vanderlaan2011; @vanderlaan2018].

SDR operates through the following process:

1. **Initial Modeling:** It employs machine learning algorithms to flexibly model the relationship between treatments, covariates, and outcomes at each time point. This flexibility allows SDR to account for complex, high-dimensional covariate spaces without imposing restrictive parametric modeling assumptions.

2. **Sequential Estimation:** SDR applies a sequential regression approach, estimating the treatment effect by working backwards in time. At each time point, it combines the outcome regression and the propensity score to construct an unbiased estimating equation. This process naturally incorporates the time-dependent nature of the data and interventions.

SDR offers several advantages for causal inference:
- Sequential double robustness: The estimator is consistent if, for each time point, either the outcome model or the treatment mechanism model is correctly specified (but not necessarily both)
- Efficiency in the presence of time-varying confounding
- Ability to handle complex longitudinal data structures and time-varying treatments
- Flexibility in specifying dynamic treatment regimes
- Compatibility with machine learning methods for initial estimation steps

SDR uses cross-validation to avoid over-fitting and ensure that the estimator performs well in finite samples. The combination of SDR and machine learning technologies reduces the dependence on restrictive modeling assumptions and introduces an additional layer of robustness.

Estimation is performed using the `lmtp` package [@williams2021; @díaz2021; @hoffman2023]. For further details on the theoretical properties and practical applications of SDR, see [@hoffman2022; @hoffman2023; @díaz2021]. Graphs, tables, and output reports are created using the `margot` package [@margot2024].
    ",
    grf = "
### Generalized Random Forests (GRF)

In this study, we employ Generalized Random Forests (GRF) to estimate causal effects, using the grf package [@grf2024]. GRF extends the random forest algorithm to estimate heterogeneous treatment effects, providing a non-parametric approach to causal inference. This method allows us to estimate conditional average treatment effects (CATE) across different subgroups or individual characteristics.

GRF offers several advantages for causal inference:

1. Complex, non-linear relationships: GRF can model intricate interactions between covariates and treatment effects without requiring pre-specification.
2. Heterogeneous treatment effects: The method allows exploration of how treatment effects vary across different subgroups or individual characteristics.
3. Variable importance measures: GRF provides insights into which variables most influence treatment effects.
4. Flexibility with outcome types: It can handle continuous, binary, and time-to-event outcomes without assuming specific data distributions.
5. Double robustness: GRF estimates remain consistent if either the outcome model or the propensity score model is correctly specified (but not necessarily both).
6. Out-of-bag predictions: By using out-of-bag samples for fitting, GRF helps prevent overfitting and provides honest treatment effect estimates.

The GRF method combines machine learning flexibility with statistical inference rigor, making it particularly useful in complex settings where treatment effects may vary across subpopulations or when relationships between covariates and outcomes are expected to be non-linear.

In our analysis:

We use the `maq `package [@maq_package_2024] to assess treatment heterogeneity.
We evaluate optimal policies using the `policytree` package [@policytree_package_2024; @athey_2021_policy_tree_econometrica].
We visualize policy predictions on held-out data using the `margot` package [@margot2024].

By employing GRF, we can estimate both average treatment effects and explore how these effects differ across various subgroups or individual characteristics, providing clearer insight into causal relationships in the target population.
    "
  )

  selected_estimators <- estimator_texts[unlist(estimators)]

  markdown_text <- paste0("
### Statistical Estimator
  ",
                          paste(unlist(selected_estimators), collapse = "\n\n")
  )

  return(markdown_text)
}
