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
  # Input validation and conversion to data.table
  dt <- as.data.table(copy(dt))

  # Validate required variables exist
  required_vars <- c(cluster_id, id_var, wave_var, condition_var, year_measured_var)
  missing_required <- setdiff(required_vars, names(dt))
  if (length(missing_required) > 0) {
    cli::cli_abort("Missing required variables: {paste(missing_required, collapse = ', ')}")
  }

  # Ensure wave_var is numeric
  if (!is.numeric(dt[[wave_var]])) {
    dt[, (wave_var) := as.numeric(as.character(get(wave_var)))]
    if (any(is.na(dt[[wave_var]]))) {
      cli::cli_abort("The '{wave_var}' variable cannot be converted to numeric.")
    }
  }

  # Get summary statistics before transformation
  total_clusters <- uniqueN(dt[[cluster_id]])
  total_ids <- uniqueN(dt[[id_var]])
  wave_range <- range(dt[[wave_var]])
  cli::cli_alert_info("Dataset contains {total_clusters} clusters, {total_ids} unique IDs, waves {wave_range[1]} to {wave_range[2]}")

  # Order data by cluster and wave
  setorderv(dt, cols = c(cluster_id, wave_var))

  # For each wave, identify clusters where condition is met
  dt[, should_transform := {
    any_zero <- any(get(condition_var) == condition_value, na.rm = TRUE)
    if(any_zero) .I else integer(0)
  }, by = .(get(wave_var), get(cluster_id))]

  # Identify rows for both conditions
  condition_met_rows <- dt[get(condition_var) == condition_value, .I]
  condition_not_met_rows <- dt[get(condition_var) == 1, .I]

  # Calculate statistics for both conditions by wave
  transform_stats <- dt[,
                        .(
                          zeros = sum(get(condition_var) == condition_value, na.rm = TRUE),
                          ones = sum(get(condition_var) == 1, na.rm = TRUE),
                          clusters_with_zeros = uniqueN(get(cluster_id)[get(condition_var) == condition_value]),
                          clusters_with_ones = uniqueN(get(cluster_id)[get(condition_var) == 1]),
                          ids_with_zeros = uniqueN(get(id_var)[get(condition_var) == condition_value]),
                          ids_with_ones = uniqueN(get(id_var)[get(condition_var) == 1])
                        ),
                        by = get(wave_var)]

  setnames(transform_stats, "get", "wave")
  setorder(transform_stats, wave)

  # Apply transformation
  if (length(condition_met_rows) > 0) {
    dt[condition_met_rows, (year_measured_var) := 0]

    # Print detailed statistics
    cli::cli_h2("Detailed Statistics by Wave")

    # Fixed the wave statistics printing
    for (i in seq_len(nrow(transform_stats))) {
      stats <- transform_stats[i]
      cli::cli_alert_info(sprintf(
        "Wave %d:\n  - %d rows with %s == 0\n  - %d rows with %s == 1\n  - %d clusters with %s == 0\n  - %d clusters with %s == 1",
        stats$wave,
        stats$zeros,
        condition_var,
        stats$ones,
        condition_var,
        stats$clusters_with_zeros,
        condition_var,
        stats$clusters_with_ones,
        condition_var
      ))
    }

    # Calculate clusters with both conditions
    clusters_both <- dt[, .(
      has_zero = any(get(condition_var) == condition_value),
      has_one = any(get(condition_var) == 1)
    ), by = get(cluster_id)][has_zero == TRUE & has_one == TRUE, .N]

    # Calculate overall statistics
    affected_clusters_zero <- uniqueN(dt[condition_met_rows, get(cluster_id)])
    affected_clusters_one <- uniqueN(dt[condition_not_met_rows, get(cluster_id)])
    affected_ids_zero <- uniqueN(dt[condition_met_rows, get(id_var)])
    affected_ids_one <- uniqueN(dt[condition_not_met_rows, get(id_var)])
    total_transformed <- length(condition_met_rows)
    total_not_transformed <- length(condition_not_met_rows)

    cli::cli_alert_success("Comprehensive transformation summary:")
    cli::cli_alert_info(sprintf("Clusters:"))
    cli::cli_alert_info(sprintf("- %d clusters had at least one %s == 0",
                                affected_clusters_zero, condition_var))
    cli::cli_alert_info(sprintf("- %d clusters had at least one %s == 1",
                                affected_clusters_one, condition_var))
    cli::cli_alert_info(sprintf("- %d clusters had both conditions",
                                clusters_both))

    cli::cli_alert_info(sprintf("\nUnique IDs:"))
    cli::cli_alert_info(sprintf("- %d unique IDs had %s == 0",
                                affected_ids_zero, condition_var))
    cli::cli_alert_info(sprintf("- %d unique IDs had %s == 1",
                                affected_ids_one, condition_var))

    cli::cli_alert_info(sprintf("\nTotal Rows:"))
    cli::cli_alert_info(sprintf("- %d rows transformed (where %s == 0)",
                                total_transformed, condition_var))
    cli::cli_alert_info(sprintf("- %d rows not transformed (where %s == 1)",
                                total_not_transformed, condition_var))
  } else {
    cli::cli_alert_info(sprintf("No transformations were necessary (no rows with %s == %d)",
                                condition_var, condition_value))
  }

  # Remove temporary column
  dt[, should_transform := NULL]

  return(dt)
}
