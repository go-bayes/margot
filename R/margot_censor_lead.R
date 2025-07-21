#' Apply Lead-Based Censoring to Longitudinal Data
#'
#' @param dt A data.frame or data.table in long format containing repeated measures.
#' @param cluster_id A string specifying the cluster identifier variable. If NULL, defaults to id_var.
#' @param id_var A string specifying the individual identifier variable. Default is "id".
#' @param wave_var A string specifying the variable indicating the wave. Default is "wave".
#' @param condition_var A string specifying the condition variable to create lead for.
#' @param condition_value The value of lead condition_var that triggers transformation. Default is 0.
#' @param year_measured_var A string specifying the year_measured variable. Default is "year_measured".
#' @param cluster_condition String specifying cluster censoring logic: "ANY" (default) or "ALL".
#'
#' @return A modified data.table with updated year_measured values based on lead conditions.
#'
#' @importFrom data.table as.data.table copy set uniqueN setorderv shift
#' @importFrom cli cli_abort cli_alert_info cli_alert_success
#' @export
margot_censor_lead <- function(
    dt,
    cluster_id = NULL,
    id_var = "id",
    wave_var = "wave",
    condition_var,
    condition_value = 0,
    year_measured_var = "year_measured",
    cluster_condition = "ANY"
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }
  library(data.table)
  .datatable.aware <- TRUE
  dt <- data.table::as.data.table(data.table::copy(dt))

  # default cluster_id to id_var if not specified
  if (is.null(cluster_id)) {
    cluster_id <- id_var
  }

  # check required variables
  required_vars <- c(cluster_id, id_var, wave_var, condition_var, year_measured_var)
  missing_required <- setdiff(required_vars, names(dt))
  if (length(missing_required) > 0) {
    cli::cli_abort("Missing required variables: {paste(missing_required, collapse = ', ')}")
  }

  # ensure wave is numeric
  if (!is.numeric(dt[[wave_var]])) {
    data.table::set(dt, j = wave_var, value = as.numeric(as.character(dt[[wave_var]])))
    if (any(is.na(dt[[wave_var]]))) {
      cli::cli_abort("The '{wave_var}' variable cannot be converted to numeric.")
    }
  }

  # validate cluster_condition parameter
  if (!cluster_condition %in% c("ANY", "ALL")) {
    cli::cli_abort("cluster_condition must be either 'ANY' or 'ALL'")
  }

  # sort data
  data.table::setorderv(dt, c(cluster_id, id_var, wave_var))

  # create lead variable within each id group
  dt[, lead_var_temp := data.table::shift(get(condition_var), n = 1L, type = "lead"),
     by = get(id_var)]

  # identify dataset characteristics
  total_clusters <- data.table::uniqueN(dt[[cluster_id]])
  total_ids <- data.table::uniqueN(dt[[id_var]])
  wave_range <- range(dt[[wave_var]], na.rm = TRUE)
  final_wave <- max(dt[[wave_var]], na.rm = TRUE)

  cli::cli_alert_info("Dataset has {total_clusters} clusters, {total_ids} unique IDs, waves {wave_range[1]} to {wave_range[2]}")

  # find minimum trigger wave for each cluster (vectorized approach)
  trigger_data <- dt[get(wave_var) != final_wave &
                       !is.na(lead_var_temp) &
                       lead_var_temp == condition_value,
                     .(min_trigger_wave = min(get(wave_var))),
                     by = get(cluster_id)]

  if (nrow(trigger_data) == 0) {
    cli::cli_alert_info("No lead conditions met for censoring.")
    dt[, lead_var_temp := NULL]
    return(dt)
  }

  data.table::setnames(trigger_data, c("cluster", "min_trigger_wave"))

  # handle cluster_condition = "ALL" logic
  if (cluster_condition == "ALL") {
    # count total members per cluster
    cluster_sizes <- dt[, data.table::uniqueN(get(id_var)), by = get(cluster_id)]
    data.table::setnames(cluster_sizes, c("cluster", "total_members"))

    # count members with trigger condition per cluster
    trigger_members <- dt[get(wave_var) != final_wave &
                            !is.na(lead_var_temp) &
                            lead_var_temp == condition_value,
                          data.table::uniqueN(get(id_var)),
                          by = get(cluster_id)]
    data.table::setnames(trigger_members, c("cluster", "trigger_members"))

    # merge and filter to clusters where all members have trigger
    cluster_summary <- merge(cluster_sizes, trigger_members, by = "cluster", all.x = TRUE)
    cluster_summary[is.na(trigger_members), trigger_members := 0L]
    valid_clusters <- cluster_summary[total_members == trigger_members, cluster]

    if (length(valid_clusters) == 0) {
      cli::cli_alert_info("No clusters have ALL members meeting the lead condition.")
      dt[, lead_var_temp := NULL]
      return(dt)
    }

    trigger_data <- trigger_data[cluster %in% valid_clusters]
  }

  # vectorized censoring approach
  # merge trigger info back to main dataset
  dt_with_triggers <- merge(dt, trigger_data,
                            by.x = cluster_id, by.y = "cluster",
                            all.x = TRUE, sort = FALSE)

  # create censoring indicator vectorially
  censor_idx <- !is.na(dt_with_triggers$min_trigger_wave) &
    dt_with_triggers[[wave_var]] >= (dt_with_triggers$min_trigger_wave + 1L)

  censored_observations <- sum(censor_idx)

  # apply censoring in bulk
  if (censored_observations > 0) {
    # set year_measured to 0 for censored observations
    dt[censor_idx, (year_measured_var) := 0]

    # set other variables to NA (excluding key identifiers)
    vars_to_na <- setdiff(names(dt),
                          c(cluster_id, id_var, wave_var, year_measured_var, "lead_var_temp"))
    dt[censor_idx, (vars_to_na) := NA]
  }

  # cleanup temporary variables
  dt[, lead_var_temp := NULL]

  # report results
  cli::cli_alert_success("Applied lead censoring for {nrow(trigger_data)} cluster(s) using '{cluster_condition}' logic.")
  cli::cli_alert_info("Censored {censored_observations} observations from lead wave onwards.")
  cli::cli_alert_info("Final wave (wave = {final_wave}) exempt from triggering censoring.")

  return(dt)
}
