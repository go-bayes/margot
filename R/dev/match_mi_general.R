#' General Matching Function for Multiple Imputation Data
#'
#' This function facilitates propensity score matching on datasets, including those
#' generated through multiple imputation, to assess covariate balance across treatment
#' groups. It leverages the \code{\link[WeightIt]{WeightIt}} and
#' \code{\link[MatchThem]{MatchThem}} packages to calculate propensity scores and apply
#' matching techniques. Users are referred to these packages for detailed methodological
#' specifications and additional functionality.
#'
#' @param data A data frame or a 'mids' object containing the data for matching.
#' @param X A string naming the treatment or exposure variable within `data`.
#' @param baseline_vars A character vector naming the covariates to include in the
#'   propensity score model.
#' @param estimand A string specifying the target estimand ('ATE', 'ATT', or 'ATC').
#' @param method The method for estimating propensity scores, per the
#'   \code{\link[WeightIt]{WeightIt}} package's documentation.
#' @param subgroup (Optional) A string specifying a variable by which to subgroup
#'   the data for within-group matching.
#' @param focal (Optional) Specifies the focal treatment group, useful for 'ATT' or
#'   'ATC' estimands.
#' @param sample_weights (Optional) A string indicating the variable in `data`
#'   representing sample weights.
#' @param stabilize Logical. Indicates whether to stabilize weights, defaulting to TRUE.
#'
#' @return A list of matched datasets for each subgroup level (if `subgroup` is used)
#'   or a single matched dataset. Each element is typically a data frame or an object
#'   reflecting the matched data structure, depending on the matching method applied.
#'
#' @examples
#' \dontrun{
#' # Assume `df` is a data frame with treatment, covariates, and sample weights
#' matched_data <- match_mi_general(data = df,
#'                                  X = "treatment_var",
#'                                  baseline_vars = c("covariate1", "covariate2"),
#'                                  estimand = "ATE",
#'                                  method = "nearest",
#'                                  stabilize = TRUE)
#' }
#'
#' @importFrom WeightIt weightit
#' @importFrom MatchThem weightthem
#' @references
#' Detailed methodology and additional options can be found in:
#' - \code{\link[WeightIt]{WeightIt}} package for propensity score estimation.
#' - \code{\link[MatchThem]{MatchThem}} package for matching within imputed datasets.
#' @keywords internal
match_mi_general <-
  function(data,
           X,
           baseline_vars,
           estimand,
           method,
           subgroup = NULL,
           focal = NULL,
           sample_weights = NULL) {

    data_class <- class(data)

    if (!data_class %in% c("mids", "data.frame")) {
      stop("Input data must be either 'mids' or 'data.frame' object")
    }

    formula_str <- as.formula(paste(X, "~", paste(baseline_vars, collapse = "+")))

    weight_function <- if (data_class == "mids") MatchThem::weightthem else WeightIt::weightit

    perform_matching <- function(data_subset) {
      if (is.null(sample_weights)) {
        weight_function(
          formula = formula_str,
          data = data_subset,
          estimand = estimand,
          stabilize = TRUE,
          method = method,
          focal = focal
        )
      } else {
        weight_function(
          formula = formula_str,
          data = data_subset,
          estimand = estimand,
          stabilize = TRUE,
          method = method,
          sample_weights = sample_weights,
          focal = focal
        )
      }
    }

    if (is.null(subgroup)) {
      dt_match <- perform_matching(data)
    } else {
      levels_list <- unique(data[[subgroup]])

      dt_match_list <- lapply(levels_list, function(level) {
        data_subset <- data[data[[subgroup]] == level, ]
        perform_matching(data_subset)
      })

      names(dt_match_list) <- levels_list
      dt_match <- dt_match_list
    }

    return(dt_match)
  }
