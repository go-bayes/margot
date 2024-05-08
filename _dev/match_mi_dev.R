match_mi_general_dev <- function(data,
                                 X,
                                 baseline_vars,
                                 estimand,
                                 method,
                                 focal = NULL,
                                 sample_weights = NULL,
                                 stabilize = FALSE,
                                 include.obj = FALSE,
                                 keep.mparts = TRUE,
                                 ...) {

  # Check input data type
  data_class <- class(data)
  if (!data_class %in% c("mids", "data.frame")) {
    stop("Input data must be either 'mids' or 'data.frame' object")
  }

  # Construct the formula
  formula_str <- as.formula(paste(X, "~", paste(baseline_vars, collapse = "+")))

  # Choose appropriate function based on data type
  weight_function <- if (data_class == "mids") MatchThem::weightthem else WeightIt::weightit

  # Function to perform matching
  perform_matching <- function(data) {
    if (is.null(sample_weights)) {
      weight_function(
        formula = formula_str,
        data = data,
        estimand = estimand,
        stabilize = stabilize,
        method = method,
        focal = focal,
        include.obj = include.obj,
        keep.mparts = keep.mparts,
        ...
      )
    } else {
      weight_function(
        formula = formula_str,
        data = data,
        estimand = estimand,
        stabilize = stabilize,
        method = method,
        sample_weights = sample_weights,
        focal = focal,
        include.obj = include.obj,
        keep.mparts = keep.mparts,
        ...
      )
    }
  }

  # Perform matching on the entire dataset
  dt_match <- perform_matching(data)

  return(dt_match)
}
