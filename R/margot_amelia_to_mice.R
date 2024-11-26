#' convert an amelia object to a mice object
#'
#' @param amelia_output an object of class `amelia`, containing imputed datasets from the Amelia package
#'
#' @return a `mids` object compatible with the `mice` package, structured with the original dataset and imputed values
#' @export
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
