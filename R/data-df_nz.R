
#' @docType data
#' @title df_nz Synthetic Dataset
#' @name df_nz
#' A synthetic dataset containing columns of responses measured on 20,000 ids by wave. This dataset
#' is designed for teaching and illustrations.
#'
#' @description A data frame with 20,000 observations on the following 4 variables:
#' \describe{
#'   \item{\code{id}}{Unique identifier for respondents. Factor}
#'   \item{\code{wave}}{Wave of the survey. Factor with levels 1, 2, 3.}
#'   \item{\code{agreeableness}}{Agreeableness scores. Numeric.}
#'   \item{\code{...}}{Other variables not detailed here.}
#' }
#'
#' @source Synthetically generated for teaching purposes.
#'
#' @examples
#' # Load the dataset
#' data("df_nz")
#'
#' # View the first few rows of the dataset
#' head(df_nz)
#'
#' # Summary of agreeableness responses by wave
#' library(dplyr)
#' df_nz %>%
#'   select(id, wave, agreeableness) %>%
#'   group_by(wave) %>%
#'   summarise(mean_agreeableness = mean(agreeableness, na.rm = TRUE))

