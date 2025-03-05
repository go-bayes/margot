#' Transform year_measured Variable Based on Clustered Conditions Within Waves
#'
#' @param dt A `data.frame` or `data.table` in long format containing repeated measures.
#' @param cluster_id A string specifying the cluster identifier variable. Default is "rel_num".
#' @param id_var A string specifying the individual identifier variable. Default is "id".
#' @param wave_var A string specifying the variable indicating the wave. Default is "wave".
#' @param condition_var A string specifying the condition variable. Default is "rel_complete".
#' @param condition_value The value of `condition_var` that triggers transformation. Default is 0.
#' @param year_measured_var A string specifying the `year_measured` variable. Default is "year_measured".
#'
#' @return A modified `data.table` with updated year_measured values based on the condition.
#'
#' @import data.table
#' @import cli
#' @export
#' Transform year_measured Variable Based on Clustered Conditions Within Waves
#'
#' @param dt A `data.frame` or `data.table` in long format containing repeated measures.
#' @param cluster_id A string specifying the cluster identifier variable. Default is "rel_num".
#' @param id_var A string specifying the individual identifier variable. Default is "id".
#' @param wave_var A string specifying the variable indicating the wave. Default is "wave".
#' @param condition_var A string specifying the condition variable. Default is "rel_complete".
#' @param condition_value The value of `condition_var` that triggers transformation. Default is 0.
#' @param year_measured_var A string specifying the `year_measured` variable. Default is "year_measured".
#'
#' @return A modified `data.table` with updated year_measured values based on the condition.
#'
#' @import data.table
#' @import cli
#' @export
margot_censor <- function(
    dt,
    cluster_id = "rel_num",
    id_var = "id",
    wave_var = "wave",
    condition_var = "rel_complete",
    condition_value = 0,
    year_measured_var = "year_measured"
) {
  # Ensure data.table is loaded
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  library(data.table)
  library(cli)

  .datatable.aware <- TRUE

  # Convert dt to data.table
  dt <- as.data.table(copy(dt))

  # Check required variables
  required_vars <- c(cluster_id, id_var, wave_var, condition_var, year_measured_var)
  missing_required <- setdiff(required_vars, names(dt))
  if (length(missing_required) > 0) {
    cli_abort("Missing required variables: {paste(missing_required, collapse = ', ')}")
  }

  # Ensure wave_var is numeric
  if (!is.numeric(dt[[wave_var]])) {
    set(dt, j = wave_var, value = as.numeric(as.character(dt[[wave_var]])))
    if (any(is.na(dt[[wave_var]]))) {
      cli_abort("The '{wave_var}' variable cannot be converted to numeric.")
    }
  }

  # Basic summary
  total_clusters <- uniqueN(dt[[cluster_id]])
  total_ids <- uniqueN(dt[[id_var]])
  wave_range <- range(dt[[wave_var]])
  cli::cli_alert_info("Dataset has {total_clusters} clusters, {total_ids} unique IDs, waves {wave_range[1]} to {wave_range[2]}")

  # 1. Identify clusters that have ANY row with condition == condition_value
  clusters_to_censor <- dt[get(condition_var) == condition_value, unique(get(cluster_id))]

  if (length(clusters_to_censor) == 0) {
    cli_alert_info("No clusters meet the censoring condition ({condition_var} == {condition_value}).")
    return(dt)
  }

  # 2. Set year_measured_var = 0 in those clusters
  dt[get(cluster_id) %in% clusters_to_censor, (year_measured_var) := 0]

  # 3. Set all other variables to NA
  #    (preserve cluster_id, wave_var, id_var, condition_var, and year_measured_var)
  vars_to_na <- setdiff(
    names(dt),
    c(cluster_id, wave_var, id_var, condition_var, year_measured_var)
  )
  dt[get(cluster_id) %in% clusters_to_censor, (vars_to_na) := NA]

  # Print summary
  cli_alert_success("Applied full cluster-level censoring for {length(clusters_to_censor)} cluster(s).")
  cli_alert_info("All variables (except {cluster_id}, {wave_var}, {id_var}, {condition_var}, {year_measured_var}) are now NA where {condition_var} == {condition_value} in a cluster.")

  return(dt)
}
