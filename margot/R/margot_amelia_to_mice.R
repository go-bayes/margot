#' convert an amelia object to a mice object
#'
#' @param amelia_output an object of class `amelia`, containing imputed datasets from the Amelia package
#'
#' @return a `mids` object compatible with the `mice` package, structured with the original dataset and imputed values
#' @keywords internal
#'
#' @examples
#' # load Amelia package and perform imputation
#' library(Amelia)
#' data(africa) # example dataset from Amelia package
#' amelia_output <- amelia(x = africa, m = 5, idvars = "country") # impute data
#'
#' # convert amelia object to mice object
#' mids_obj <- margot_amelia_to_mice(amelia_output)
#'
#' # verify mids object
#' print(mids_obj)
margot_amelia_to_mice <- function(amelia_obj) {
  # Verify input is an amelia object
  if (!inherits(amelia_obj, "amelia")) {
    stop("Input must be an amelia object")
  }

  # Get imputations and combine with original data
  imp_list <- amelia_obj$imputations

  # Initialize mice format list
  mice_format <- list()

  # Set original data (first imputation)
  mice_format$data <- imp_list[[1]]
  mice_format$data$.imp <- 0

  # Create where matrix - use complete.cases on original data
  mice_format$where <- matrix(FALSE,
                              nrow = nrow(mice_format$data),
                              ncol = ncol(mice_format$data))
  colnames(mice_format$where) <- names(mice_format$data)

  # Get all variables except special columns
  all_vars <- setdiff(names(mice_format$data), c(".imp", ".id"))

  # Find imputed variables by comparing imputations
  imputed_vars <- names(which(colSums(imp_list[[1]] != imp_list[[2]]) > 0))
  imputed_vars <- setdiff(imputed_vars, c(".imp", ".id"))

  # Create imp array
  n_imp <- length(imp_list)
  mice_format$imp <- vector("list", length(imputed_vars))
  names(mice_format$imp) <- imputed_vars

  # Fill imp array
  for (var in imputed_vars) {
    mice_format$imp[[var]] <- matrix(NA,
                                     nrow = nrow(mice_format$data),
                                     ncol = n_imp)
    for (m in 1:n_imp) {
      mice_format$imp[[var]][, m] <- imp_list[[m]][[var]]
    }
  }

  # Set mice attributes
  mice_format$m <- n_imp
  mice_format$method <- rep("amelia", length(imputed_vars))
  names(mice_format$method) <- imputed_vars
  mice_format$visitSequence <- imputed_vars
  mice_format$iteration <- 1
  mice_format$seed <- NA
  mice_format$version <- "3.0"
  class(mice_format) <- "mids"

  return(mice_format)
}


## WRONG DO THIS

# survey-based functions for repeated measures + multiple imputation
# uses survey::svydesign with clustering instead of geepack
#
# # helper function to extract complete datasets from amelia-converted mids objects
# extract_amelia_complete <- function(mids_obj, action) {
#   # start with original data
#   complete_data <- mids_obj$data
#
#   # replace imputed variables with values from specified imputation
#   imputed_vars <- names(mids_obj$imp)
#
#   for (var in imputed_vars) {
#     if (var %in% names(complete_data)) {
#       # replace with imputed values from specified action
#       complete_data[[var]] <- mids_obj$imp[[var]][, action]
#     }
#   }
#
#   # remove .imp column if it exists
#   complete_data$.imp <- NULL
#
#   return(complete_data)
# }
#
# # helper operator for null handling
# `%||%` <- function(x, y) if (is.null(x)) y else x
#
# # main function: survey design + clustering for repeated measures
# fit_survey_mice <- function(
#     mids_obj,
#     formula,
#     id_col,
#     weight_col,
#     model_type = c("linear", "multinomial", "ordinal"),
#     n_imputations = NULL,
#     prediction_terms = NULL,
#     family = gaussian(),
#     return_individual = FALSE
# ) {
#
#   # validate inputs
#   is_valid_mids <- inherits(mids_obj, "mids") || inherits(mids_obj, "simple_mids")
#   if (!is_valid_mids) {
#     stop("mids_obj must be a mids object from mice package or simple_mids")
#   }
#
#   model_type <- match.arg(model_type)
#
#   # check required packages
#   if (!requireNamespace("survey", quietly = TRUE)) {
#     stop("survey package required. install with: install.packages('survey')")
#   }
#
#   if (model_type %in% c("multinomial", "ordinal")) {
#     if (!requireNamespace("nnet", quietly = TRUE) && model_type == "multinomial") {
#       stop("nnet package required for multinomial models")
#     }
#     if (!requireNamespace("MASS", quietly = TRUE) && model_type == "ordinal") {
#       stop("MASS package required for ordinal models")
#     }
#   }
#
#   # determine number of imputations
#   if (inherits(mids_obj, "simple_mids")) {
#     m <- n_imputations %||% mids_obj$m
#     if (m > mids_obj$m) {
#       warning(paste("requested", m, "imputations but only", mids_obj$m, "available. using", mids_obj$m))
#       m <- mids_obj$m
#     }
#   } else {
#     m <- n_imputations %||% mids_obj$m
#     if (m > mids_obj$m) {
#       warning(paste("requested", m, "imputations but only", mids_obj$m, "available. using", mids_obj$m))
#       m <- mids_obj$m
#     }
#   }
#
#   # check columns exist in first dataset
#   if (inherits(mids_obj, "simple_mids")) {
#     first_data <- mids_obj$complete_data_list[[1]]
#   } else {
#     # use our custom function for amelia-converted mids objects
#     first_data <- extract_amelia_complete(mids_obj, 1)
#   }
#
#   required_cols <- c(weight_col, id_col)
#   missing_cols <- setdiff(required_cols, names(first_data))
#   if (length(missing_cols) > 0) {
#     stop(paste("columns not found in imputed data:", paste(missing_cols, collapse = ", ")))
#   }
#
#   # check formula variables
#   formula_vars <- all.vars(formula)
#   missing_vars <- setdiff(formula_vars, names(first_data))
#   if (length(missing_vars) > 0) {
#     stop(paste("formula variables not found:", paste(missing_vars, collapse = ", ")))
#   }
#
#   # fit models for each imputation
#   models_list <- vector("list", m)
#   predictions_list <- vector("list", m)
#
#   for (i in 1:m) {
#     cat("fitting imputation", i, "of", m, "\n")
#
#     # extract complete dataset
#     if (inherits(mids_obj, "simple_mids")) {
#       data_i <- mids_obj$complete_data_list[[i]]
#     } else {
#       # use our custom function for amelia-converted mids objects
#       data_i <- extract_amelia_complete(mids_obj, i)
#     }
#
#     # create survey design with clustering for repeated measures
#     design_i <- survey::svydesign(
#       ids = reformulate(id_col),          # cluster by individual
#       weights = reformulate(weight_col),   # survey weights
#       data = data_i
#     )
#
#     # fit appropriate model
#     tryCatch({
#       if (model_type == "linear") {
#         # survey-weighted linear model
#         model_i <- survey::svyglm(
#           formula = formula,
#           design = design_i,
#           family = family
#         )
#       } else if (model_type == "multinomial") {
#         # weighted multinomial (survey doesn't have svymultinom)
#         model_i <- nnet::multinom(
#           formula = formula,
#           data = data_i,
#           weights = data_i[[weight_col]],
#           trace = FALSE
#         )
#       } else if (model_type == "ordinal") {
#         # survey ordinal logistic
#         model_i <- survey::svyolr(
#           formula = formula,
#           design = design_i
#         )
#       }
#
#       models_list[[i]] <- model_i
#
#       # generate predictions if requested (separate try-catch for each type)
#       if (!is.null(prediction_terms)) {
#         if (model_type == "multinomial") {
#           # for multinomial: try cluster-robust, fallback to standard
#           cluster_vector <- data_i[[id_col]]  # extract before tryCatch
#           tryCatch({
#             pred_i <- ggeffects::predict_response(
#               model_i,
#               terms = prediction_terms,
#               margin = "marginalmeans",
#               vcov_args = list(type = "CR", cluster = cluster_vector)
#             )
#             predictions_list[[i]] <- pred_i
#           }, error = function(e) {
#             tryCatch({
#               pred_i <- ggeffects::ggpredict(model_i, terms = prediction_terms)
#               predictions_list[[i]] <- pred_i
#               warning(paste("using standard se for imputation", i))
#             }, error = function(e2) {
#               warning(paste("prediction failed for imputation", i, ":", e2$message))
#               predictions_list[[i]] <- NULL
#             })
#           })
#         } else {
#           # for survey models (linear, ordinal)
#           tryCatch({
#             pred_i <- ggeffects::ggpredict(model_i, terms = prediction_terms)
#             predictions_list[[i]] <- pred_i
#           }, error = function(e) {
#             warning(paste("prediction failed for imputation", i, ":", e$message))
#             predictions_list[[i]] <- NULL
#           })
#         }
#       }
#
#     }, error = function(e) {
#       cat("error fitting model for imputation", i, ":\n")
#       cat("- error message:", e$message, "\n")
#       cat("- data dimensions:", dim(data_i), "\n")
#       cat("- design variables: id =", id_col, ", weights =", weight_col, "\n")
#       stop(paste("model fitting failed for imputation", i))
#     })
#   }
#
#   # pool predictions if generated
#   pooled_predictions <- NULL
#   if (!is.null(prediction_terms)) {
#     valid_predictions <- predictions_list[!sapply(predictions_list, is.null)]
#     if (length(valid_predictions) > 0) {
#       pooled_predictions <- ggeffects::pool_predictions(valid_predictions)
#     } else {
#       warning("no valid predictions generated")
#     }
#   }
#
#   # return results
#   result <- list(
#     models = models_list,
#     pooled_predictions = pooled_predictions,
#     n_imputations = m,
#     formula = formula,
#     model_type = model_type,
#     id_col = id_col,
#     weight_col = weight_col
#   )
#
#   if (return_individual) {
#     result$individual_predictions <- predictions_list
#   }
#
#   class(result) <- "survey_mice_result"
#   return(result)
# }
#
# # convenience wrappers for different model types
# fit_survey_linear_mice <- function(mids_obj, formula, id_col, weight_col,
#                                    prediction_terms = NULL, n_imputations = NULL,
#                                    family = gaussian(), return_individual = FALSE) {
#   fit_survey_mice(
#     mids_obj = mids_obj,
#     formula = formula,
#     id_col = id_col,
#     weight_col = weight_col,
#     model_type = "linear",
#     prediction_terms = prediction_terms,
#     n_imputations = n_imputations,
#     family = family,
#     return_individual = return_individual
#   )
# }
#
# fit_survey_multinomial_mice <- function(mids_obj, formula, id_col, weight_col,
#                                         prediction_terms = NULL, n_imputations = NULL,
#                                         return_individual = FALSE) {
#   fit_survey_mice(
#     mids_obj = mids_obj,
#     formula = formula,
#     id_col = id_col,
#     weight_col = weight_col,
#     model_type = "multinomial",
#     prediction_terms = prediction_terms,
#     n_imputations = n_imputations,
#     return_individual = return_individual
#   )
# }
#
# fit_survey_ordinal_mice <- function(mids_obj, formula, id_col, weight_col,
#                                     prediction_terms = NULL, n_imputations = NULL,
#                                     return_individual = FALSE) {
#   fit_survey_mice(
#     mids_obj = mids_obj,
#     formula = formula,
#     id_col = id_col,
#     weight_col = weight_col,
#     model_type = "ordinal",
#     prediction_terms = prediction_terms,
#     n_imputations = n_imputations,
#     return_individual = return_individual
#   )
# }
#
# # plotting function
# plot_survey_mice_result <- function(
#     survey_result,
#     outcome_label = NULL,
#     x_label = "years: 2019-2024",
#     y_label = NULL,
#     title = NULL,
#     y_limits = NULL,
#     point_color = "red",
#     ci_color = "red",
#     theme_choice = theme_classic()
# ) {
#
#   if (is.null(survey_result$pooled_predictions)) {
#     stop("no pooled predictions available in result object")
#   }
#
#   pooled_pred <- survey_result$pooled_predictions
#
#   # determine confidence interval column names
#   ymin_col <- if ("conf.low" %in% names(pooled_pred)) "conf.low" else "ci.low"
#   ymax_col <- if ("conf.high" %in% names(pooled_pred)) "conf.high" else "ci.high"
#
#   # create base plot
#   p <- ggplot(pooled_pred, aes(x = x, y = predicted)) +
#     # add confidence intervals
#     geom_errorbar(
#       aes(ymin = .data[[ymin_col]], ymax = .data[[ymax_col]]),
#       width = 0.3,
#       colour = ci_color,
#       linetype = "dashed",
#       alpha = 0.8,
#       linewidth = 1
#     ) +
#     # add predicted points
#     geom_point(
#       colour = "white",
#       fill = point_color,
#       size = 3,
#       alpha = 1,
#       shape = 21,
#       stroke = 1.5
#     ) +
#     theme_choice
#
#   # add labels
#   if (!is.null(x_label)) p <- p + xlab(x_label)
#   if (!is.null(y_label)) p <- p + ylab(y_label)
#   if (!is.null(title)) p <- p + ggtitle(title)
#   if (!is.null(y_limits)) p <- p + scale_y_continuous(limits = y_limits)
#
#   return(p)
# }
#
# # print method
# print.survey_mice_result <- function(x, ...) {
#   cat("survey multiple imputation results\n")
#   cat("==================================\n")
#   cat("model type:", x$model_type, "\n")
#   cat("formula:", deparse(x$formula), "\n")
#   cat("number of imputations:", x$n_imputations, "\n")
#   cat("id variable:", x$id_col, "\n")
#   cat("weight variable:", x$weight_col, "\n")
#   if (!is.null(x$pooled_predictions)) {
#     cat("pooled predictions available\n")
#   }
#   cat("\n")
# }
#
# # test function to verify amelia-mids extraction
# test_amelia_extraction <- function(mids_obj, n_test = 3) {
#   cat("=== testing amelia-mids extraction ===\n")
#   cat("mids object class:", class(mids_obj), "\n")
#   cat("number of imputations available:", mids_obj$m, "\n")
#   cat("imputed variables:", paste(names(mids_obj$imp), collapse = ", "), "\n\n")
#
#   for (i in 1:min(n_test, mids_obj$m)) {
#     cat("testing imputation", i, ":\n")
#
#     tryCatch({
#       data_i <- extract_amelia_complete(mids_obj, i)
#       cat("✓ extraction successful\n")
#       cat("  dimensions:", dim(data_i), "\n")
#       cat("  .imp column removed:", !(".imp" %in% names(data_i)), "\n")
#
#       # check a few imputed variables for variation
#       imputed_vars <- intersect(names(mids_obj$imp), names(data_i))
#       if (length(imputed_vars) > 0) {
#         test_var <- imputed_vars[1]
#         cat("  ", test_var, "range:", round(range(data_i[[test_var]], na.rm = TRUE), 2), "\n")
#       }
#
#     }, error = function(e) {
#       cat("✗ extraction failed:", e$message, "\n")
#     })
#
#     cat("\n")
#   }
#
#   cat("extraction test completed!\n\n")
# }
#
# # diagnostic functions
# check_survey_design <- function(mids_obj, id_col, weight_col, imputation = 1) {
#   cat("=== checking survey design ===\n")
#
#   if (inherits(mids_obj, "simple_mids")) {
#     data_i <- mids_obj$complete_data_list[[imputation]]
#   } else {
#     # use our custom function for amelia-converted mids objects
#     data_i <- extract_amelia_complete(mids_obj, imputation)
#   }
#
#   cat("data dimensions:", dim(data_i), "\n")
#   cat("unique individuals:", length(unique(data_i[[id_col]])), "\n")
#   cat("total observations:", nrow(data_i), "\n")
#   cat("observations per person (mean):", round(nrow(data_i) / length(unique(data_i[[id_col]])), 1), "\n")
#
#   # check weights
#   cat("weight summary:\n")
#   print(summary(data_i[[weight_col]]))
#
#   # test survey design creation
#   tryCatch({
#     design_test <- survey::svydesign(
#       ids = reformulate(id_col),
#       weights = reformulate(weight_col),
#       data = data_i
#     )
#     cat("✓ survey design creation successful\n")
#     cat("design class:", class(design_test), "\n")
#   }, error = function(e) {
#     cat("✗ survey design creation failed:", e$message, "\n")
#   })
#
#   cat("\n")
# }
#
# # example usage
# example_usage_survey <- function() {
#   cat("=== survey-based approach for repeated measures + mi ===\n\n")
#
#   cat("# step 1: check your survey design\n")
#   cat("check_survey_design(mids_obj, id_col = 'id', weight_col = 'weights')\n\n")
#
#   cat("# step 2: linear models (continuous outcomes)\n")
#   cat("linear_result <- fit_survey_linear_mice(\n")
#   cat("  mids_obj = mids_obj,\n")
#   cat("  formula = trust_science ~ bs(years, 3),\n")
#   cat("  id_col = 'id',\n")
#   cat("  weight_col = 'weights',\n")
#   cat("  prediction_terms = 'years[all]',\n")
#   cat("  n_imputations = 10\n")
#   cat(")\n\n")
#
#   cat("# step 3: ordinal models (ordered factors)\n")
#   cat("ordinal_result <- fit_survey_ordinal_mice(\n")
#   cat("  mids_obj = mids_obj,\n")
#   cat("  formula = trust_science_factor ~ bs(years, 3),\n")
#   cat("  id_col = 'id',\n")
#   cat("  weight_col = 'weights',\n")
#   cat("  prediction_terms = 'years[all]',\n")
#   cat("  n_imputations = 10\n")
#   cat(")\n\n")
#
#   cat("# step 4: multinomial models (unordered factors)\n")
#   cat("multinom_result <- fit_survey_multinomial_mice(\n")
#   cat("  mids_obj = mids_obj,\n")
#   cat("  formula = some_factor ~ bs(years, 3),\n")
#   cat("  id_col = 'id',\n")
#   cat("  weight_col = 'weights',\n")
#   cat("  prediction_terms = 'years[all]',\n")
#   cat("  n_imputations = 10\n")
#   cat(")\n\n")
#
#   cat("# step 5: plot results\n")
#   cat("plot_survey_mice_result(\n")
#   cat("  linear_result,\n")
#   cat("  title = 'trust in science over time',\n")
#   cat("  y_label = 'trust in science (1-7)',\n")
#   cat("  y_limits = c(0.5, 7.5)\n")
#   cat(")\n\n")
#
#   cat("# this approach handles:\n")
#   cat("# ✓ survey weights via survey::svydesign()\n")
#   cat("# ✓ repeated measures via clustering (ids = ~id)\n")
#   cat("# ✓ multiple imputation via pooling\n")
#   cat("# ✓ ggeffects integration for predictions/plots\n")
# }
