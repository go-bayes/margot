#' Margot Example Dataset
#'
#' A small simulated dataset for demonstrating margot functionality.
#' This dataset mimics the structure of longitudinal panel data but
#' contains entirely simulated data with no real participant information.
#'
#' @format A data frame with 300 rows (100 participants × 3 waves) and 15 variables:
#' \describe{
#'   \item{id}{Participant identifier}
#'   \item{wave}{Measurement wave (0, 1, 2)}
#'   \item{year}{Year of measurement (2018, 2019, 2020)}
#'   \item{male}{Binary gender indicator (0 = not male, 1 = male)}
#'   \item{age}{Age in years}
#'   \item{partner}{Relationship status (0 = no partner, 1 = has partner)}
#'   \item{eth_cat}{Ethnicity category (euro, maori, pacific, asian)}
#'   \item{sample_weights}{Survey sampling weights}
#'   \item{forgiveness}{Forgiveness scale score (1-7)}
#'   \item{gratitude}{Gratitude scale score (1-7)}
#'   \item{modesty}{Modesty scale score (1-7)}
#'   \item{alcohol_frequency}{Frequency of alcohol consumption (0-7)}
#'   \item{alcohol_intensity}{Intensity of alcohol consumption (0-7)}
#'   \item{hours_exercise}{Hours of exercise per week}
#'   \item{ego_rubin}{Ego resilience scale (1-7)}
#' }
#'
#' @details
#' This is a lightweight example dataset suitable for package examples
#' and testing. For larger, more comprehensive simulated datasets that
#' better reflect real longitudinal studies, use `fetch_margot_data()`
#' with version = "v1" or "v2".
#'
#' @seealso 
#' \code{\link{fetch_margot_data}} for accessing larger datasets
#' \code{\link{list_margot_data}} to see all available datasets
#'
#' @examples
#' # load the example data
#' data(df_margot_example)
#' 
#' # check structure
#' str(df_margot_example)
#' 
#' # basic summary by wave
#' table(df_margot_example$wave)
#'
"df_margot_example"

#' Legacy New Zealand Attitudes and Values Study (NZAVS) Simulated Data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' This dataset is deprecated and will be removed in a future version.
#' Please use `fetch_margot_data()` instead.
#'
#' @format A data frame (see `?df_margot_example` for a similar structure)
#'
#' @details
#' This large dataset (27MB) is being phased out to reduce package size.
#' Use `fetch_margot_data(version = "v1")` to download the equivalent
#' dataset from the Open Science Framework.
#'
#' @examples
#' \dontrun{
#' # old approach (deprecated)
#' data(df_nz)
#' 
#' # new approach
#' df <- fetch_margot_data(version = "v1")
#' }
#'
#' @seealso \code{\link{fetch_margot_data}}
"df_nz"