#' Create multiple summary tables for different variable sets across waves
#'
#' @param data data frame containing the longitudinal data
#' @param vars list of variable lists, named by table (e.g., list(baseline = c("age", "sex"), outcomes = c("health", "wellbeing")))
#' @param waves list of wave lists, named corresponding to vars (e.g., list(baseline = c(1,2), outcomes = c(2,3)))
#' @param labels named vector of variable labels (e.g., c("sdo" = "SDO", "born_nz" = "Born NZ"))
#' @param show_progress logical, whether to show a progress bar (default: TRUE)
#'
#' @return list of gtsummary tables
#' @export
#'
#' @examples
#' # define variable labels
#' var_labels <- c(
#'   "sdo" = "Social Dominance Orientation",
#'   "born_nz" = "Born NZ",
#'   "rural_gch_2018_l" = "Rural Gch 2018 Levels",
#'   "eth_cat" = "Ethnicity",
#'   "rwa" = "Right Wing Authoritarianism",
#'   "support" = "Social Support (perceived)"
#' )
#'
#' # create named lists of variables and waves
#' vars_list <- list(
#'   baseline = c("age", "sex"),
#'   outcomes = c("health", "wellbeing")
#' )
#'
#' waves_list <- list(
#'   baseline = c(1, 2),
#'   outcomes = c(2, 3)
#' )
#'
#' # create tables
#' summary_tables <- margot_make_tables(
#'   data = dat_long_amelia,
#'   vars = vars_list,
#'   waves = waves_list,
#'   labels = var_labels
#' )
#'
margot_make_tables <- function(data, vars, waves, labels = NULL, show_progress = TRUE) {
  # load required packages
  require(dplyr)
  require(gtsummary)
  require(janitor)
  require(labelled)
  require(cli)
  require(stringr)

  # function to format variable names nicely
  format_var_name <- function(name) {
    name %>%
      gsub("_", " ", .) %>%
      stringr::str_to_title() %>%
      trimws()
  }

  # validate inputs
  if (length(vars) == 0 || length(waves) == 0) {
    stop("at least one set of variables and waves must be provided")
  }

  if (length(vars) != length(waves)) {
    stop("number of variable lists must match number of wave lists")
  }

  if (length(vars) > 10) {
    stop("maximum of 10 tables allowed")
  }

  if (!all(names(vars) == names(waves))) {
    stop("names in 'vars' and 'waves' must match")
  }

  # convert labels to a list if provided
  if (!is.null(labels)) {
    labels <- as.list(labels)
  }

  # initialise progress tracking
  n_combinations <- length(vars)
  if (show_progress) {
    cli::cli_h1("Margot Make Tables")
    pb <- cli::cli_progress_bar(
      total = n_combinations,
      format = "Creating tables {cli::pb_bar} {cli::pb_percent}"
    )
  }

  # initialise results list
  tables <- list()

  # process each combination
  for (table_name in names(vars)) {
    # get current vars and waves
    curr_vars <- vars[[table_name]]
    curr_waves <- waves[[table_name]]

    # validate current combination
    if (length(curr_vars) == 0 || length(curr_waves) == 0) {
      cli::cli_alert_warning("Skipping empty variable or wave list for {table_name}")
      next
    }

    # create table
    dt_current <- data %>%
      dplyr::filter(wave %in% curr_waves) %>%
      dplyr::select(all_of(c(curr_vars, "wave")))

    # create labels for all variables, using provided labels or formatted defaults
    curr_labels <- lapply(curr_vars, function(var) {
      if (!is.null(labels) && var %in% names(labels)) {
        labels[[var]]
      } else {
        format_var_name(var)
      }
    })
    names(curr_labels) <- curr_vars

    current_table <- dt_current %>%
      gtsummary::tbl_summary(
        by = "wave",
        missing = "ifany",
        percent = "column",
        statistic = list(
          all_continuous() ~ c("{mean} ({sd})", "{min}, {max}", "{p25}, {p75}")
        ),
        type = list(all_continuous() ~ "continuous2"),
        label = curr_labels
      ) %>%
      gtsummary::modify_header(
        label = paste0("**", format_var_name(table_name), "**")
      ) %>%
      gtsummary::bold_labels()

    # store table
    tables[[table_name]] <- current_table

    # update progress
    if (show_progress) {
      cli::cli_progress_update(id = pb)
    }
  }

  # close progress bar
  if (show_progress) {
    cli::cli_progress_done()
    cli::cli_alert_success("Created {length(tables)} tables successfully \U0001F44D")
  }

  # return results
  return(tables)
}
