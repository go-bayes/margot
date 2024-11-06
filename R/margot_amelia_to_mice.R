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
margot_amelia_to_mice <- function(amelia_output) {
  # get imputations
  imp_list <- amelia_output$imputations

  # make empty list to store processed data
  mice_format <- list()

  # init the .id variable if it doesn't exist
  if (!".id" %in% names(imp_list[[1]])) {
    for (i in seq_along(imp_list)) {
      imp_list[[i]]$.id <- 1:nrow(imp_list[[i]])
    }
  }

  # get var names
  all_vars <- names(imp_list[[1]])

  # find imputed vars
  # compare first two imputations to find differences
  imputed_vars <- names(which(colSums(imp_list[[1]] != imp_list[[2]]) > 0))
  imputed_vars <- setdiff(imputed_vars, c(".imp", ".id")) # remove unnecessary variables

  # make mice format
  mice_format$data <- imp_list[[1]] # original dataset
  mice_format$data$.imp <- 0        # original data marked as imp=0

  # init where matrix
  mice_format$where <- is.na(mice_format$data)

  # make imp array
  n_imp <- length(imp_list)
  n_vars <- length(imputed_vars)
  n_rows <- nrow(mice_format$data)

  # init imp array
  mice_format$imp <- vector("list", length(imputed_vars))
  names(mice_format$imp) <- imputed_vars

  # fill imp array
  for (var in imputed_vars) {
    mice_format$imp[[var]] <- matrix(NA, nrow = n_rows, ncol = n_imp)
    for (m in 1:n_imp) {
      mice_format$imp[[var]][, m] <- imp_list[[m]][[var]]
    }
  }

  # add mice attributes
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
