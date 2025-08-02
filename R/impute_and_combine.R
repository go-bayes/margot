#' Perform multiple imputation on a list of data frames and combine the results
#'
#' This function takes a list of data frames, performs multiple imputation to fill in missing
#' values using the 'mice' package, and combines the imputed datasets into a single dataset.
#' The imputations are performed separately for each data frame in the list, and the results
#' are combined into a 'mids' object, which is then cleaned and returned.
#'
#' @param list_df A list containing data frames on which to perform multiple imputation.
#' @param m The number of multiple imputations to perform for each data frame.
#' @param exclude_vars A vector of variable names to be excluded from the imputation model.
#'
#' @return A data frame that combines all imputed datasets, with unnecessary columns removed
#'         and row names reset.
#'
#' @examples
#' \dontrun{
#' # Assuming list_df is a list of data frames with missing values
#' imputed_data <- impute_and_combine(list_df, m = 5)
#' print(imputed_data)
#' }
#'
#' @importFrom mice mice complete
#' @importFrom dplyr bind_rows group_by mutate ungroup select
#' @importFrom miceadds datalist2mids
#' @export
impute_and_combine <-
  function(list_df,
           m = 10,
           exclude_vars = c("t0_sample_frame", "id", "t0_sample_origin_names_combined")) {
    # Load necessary libraries in a manner suitable for package development
    # Assumed to be loaded via namespace and described in the DESCRIPTION file
    library(mice)
    library(dplyr)
    library(miceadds)

    # Impute and complete data frames using lapply
    list_completed_df <- lapply(list_df, function(df) {
      # Create predictor matrix and exclude specified variables
      init <- mice(df, maxit = 0)
      predictorMatrix <- init$predictorMatrix
      predictorMatrix[, intersect(colnames(predictorMatrix), exclude_vars)] <- 0

      # Perform multiple imputation
      mice_df <- mice(df, m = m, method = "pmm", predictorMatrix = predictorMatrix)

      # Complete the data to 'long' format
      completed_df <- complete(mice_df, action = "long")
      rownames(completed_df) <- NULL # Reset rownames
      completed_df
    })

    # Combine all completed data frames into one
    complete_df <- bind_rows(list_completed_df)

    # Reassign .imp and .id values appropriately
    complete_df <- complete_df %>%
      group_by(.imp) %>%
      mutate(.id = row_number()) %>%
      ungroup()

    # Convert to a list of data frames by imputation
    data_list <- split(complete_df, complete_df$.imp)

    # Convert list of data frames to mids object
    mids_df <- datalist2mids(data_list, progress = FALSE)

    # Spit and Shine: Clean up the mids object as needed
    complete_data <- complete(mids_df, action = "long", include = TRUE)
    complete_data <- select(complete_data, -c(.id.1, .imp.1))
    rownames(complete_data) <- NULL

    return(complete_data)
  }
