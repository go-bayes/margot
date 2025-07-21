#' Transform year_measured Variable Based on Lead Conditions Within Waves
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
  lead_var_name <- paste0("lead_", condition_var)
  dt[, (lead_var_name) := data.table::shift(get(condition_var), n = 1L, type = "lead"),
     by = get(id_var)]

  # identify dataset characteristics
  total_clusters <- data.table::uniqueN(dt[[cluster_id]])
  total_ids <- data.table::uniqueN(dt[[id_var]])
  wave_range <- range(dt[[wave_var]], na.rm = TRUE)
  final_wave <- max(dt[[wave_var]], na.rm = TRUE)

  cli::cli_alert_info("Dataset has {total_clusters} clusters, {total_ids} unique IDs, waves {wave_range[1]} to {wave_range[2]}")

  # find waves where lead condition is met (excluding final wave since no lead exists)
  condition_met <- dt[get(wave_var) != final_wave &
                        !is.na(get(lead_var_name)) &
                        get(lead_var_name) == condition_value,
                      .(cluster = get(cluster_id),
                        id = get(id_var),
                        wave = get(wave_var))]

  if (nrow(condition_met) == 0) {
    cli::cli_alert_info("No lead conditions met for censoring.")
    dt[, (lead_var_name) := NULL]  # remove temporary lead variable
    return(dt)
  }

  # determine clusters to censor based on cluster_condition
  if (cluster_condition == "ANY") {
    # if any member of cluster meets lead condition, censor entire cluster from earliest trigger wave
    clusters_to_censor <- condition_met[, .(min_trigger_wave = min(wave)), by = cluster]
  } else {  # cluster_condition == "ALL"
    # only censor if all members of cluster meet lead condition
    cluster_member_counts <- dt[, data.table::uniqueN(get(id_var)), by = get(cluster_id)]
    data.table::setnames(cluster_member_counts, c("cluster", "total_members"))

    condition_member_counts <- condition_met[, data.table::uniqueN(id), by = cluster]
    data.table::setnames(condition_member_counts, c("cluster", "condition_members"))

    cluster_summary <- merge(cluster_member_counts, condition_member_counts, by = "cluster", all.x = TRUE)
    cluster_summary[is.na(condition_members), condition_members := 0L]

    # only clusters where all members meet condition
    clusters_all_condition <- cluster_summary[total_members == condition_members, cluster]

    if (length(clusters_all_condition) == 0) {
      cli::cli_alert_info("No clusters have ALL members meeting the lead condition.")
      dt[, (lead_var_name) := NULL]
      return(dt)
    }

    clusters_to_censor <- condition_met[cluster %in% clusters_all_condition,
                                        .(min_trigger_wave = min(wave)), by = cluster]
  }

  # apply censoring: for each cluster, censor from the wave AFTER the trigger wave onwards
  censored_observations <- 0L
  for (i in seq_len(nrow(clusters_to_censor))) {
    cluster_val <- clusters_to_censor[i, cluster]
    trigger_wave <- clusters_to_censor[i, min_trigger_wave]
    censor_start_wave <- trigger_wave + 1L  # censor from next wave onwards

    # identify rows to censor (from censor_start_wave onwards for this cluster)
    censor_idx <- dt[[cluster_id]] == cluster_val & dt[[wave_var]] >= censor_start_wave

    if (sum(censor_idx) > 0) {
      # set year_measured to 0
      dt[censor_idx, (year_measured_var) := 0]

      # set other variables to NA (excluding key identifiers and year_measured)
      vars_to_na <- setdiff(names(dt),
                            c(cluster_id, id_var, wave_var, year_measured_var, lead_var_name))
      dt[censor_idx, (vars_to_na) := NA]

      censored_observations <- censored_observations + sum(censor_idx)
    }
  }

  # remove temporary lead variable
  dt[, (lead_var_name) := NULL]

  # report results
  cli::cli_alert_success("Applied lead censoring for {nrow(clusters_to_censor)} cluster(s) using '{cluster_condition}' logic.")
  cli::cli_alert_info("Censored {censored_observations} observations from lead wave onwards.")
  cli::cli_alert_info("Final wave (wave = {final_wave}) exempt from triggering censoring.")

  return(dt)
}
