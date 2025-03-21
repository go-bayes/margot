#' Batch process heterogeneity analyses across multiple outcome domains
#'
#' @description
#' This function streamlines the process of running planned contrasts and
#' heterogeneity analyses across multiple outcome domains. It applies the same
#' subset analyses (e.g., by wealth, ethnicity, political orientation) to
#' different outcome domains, saving results, generating tables, and creating
#' visualizations.
#'
#' @param domain_models list of model sets for different domains (e.g., list(models_binary_health, models_binary_psych))
#' @param X predictor matrix used in the models
#' @param base_defaults default parameters for model processing
#' @param subset_types list of subset specifications (e.g., list(wealth = subsets_standard_wealth))
#' @param original_df original dataframe containing all variables
#' @param domain_names character vector of names for the outcome domains
#' @param subtitles character vector of subtitles for plots
#' @param output_dir directory to save outputs (defaults to NULL)
#' @param push_mods logical; whether to push models (defaults to FALSE)
#' @param interpret_all_E_gt1 logical; interpretation parameter (defaults to TRUE)
#'
#' @return a nested list containing results organized by domain and subset type
#'
#' @examples
#' # set up lists of models, names, and subtitles
#' domain_models <- list(
#'   models_binary_health,
#'   models_binary_psych,
#'   models_binary_present,
#'   models_binary_life,
#'   models_binary_social
#' )
#'
#' domain_names <- c("health", "psych", "present", "life", "social")
#'
#' subtitles <- c(
#'   subtitle_health,
#'   subtitle_psych,
#'   subtitle_present,
#'   subtitle_life,
#'   subtitle_social
#' )
#'
#' # set up subset types in a list
#' subset_types <- list(
#'   wealth = subsets_standard_wealth,
#'   ethnicity = subsets_standard_ethnicity,
#'   political = subsets_standard_political,
#'   gender = subsets_standard_gender,
#'   cohort = subsets_standard_cohort
#' )
#'
#' # run the batch processing for all domains and subsets
#' results <- margot_planned_subgroups_batch(
#'   domain_models = domain_models,
#'   X = X,
#'   base_defaults = base_defaults_binary,
#'   subset_types = subset_types,
#'   original_df = original_df,
#'   domain_names = domain_names,
#'   subtitles = subtitles,
#'   push_mods = push_mods
#' )
#'
#' @export
margot_planned_subgroups_batch <- function(
    domain_models,            # list of model sets for different domains
    X,                        # predictor matrix
    base_defaults,            # default parameters
    subset_types,             # list of subset specifications
    original_df,              # original dataframe
    domain_names,             # names for the outcome domains
    subtitles,                # subtitles for plots
    output_dir = NULL,        # directory to save outputs
    push_mods = FALSE,        # whether to push models
    interpret_all_E_gt1 = TRUE # interpretation parameter
) {
  # create a list to store results
  results_list <- list()

  # iterate through each domain
  for (i in seq_along(domain_models)) {
    domain_name <- domain_names[i]
    subtitle <- subtitles[i]

    # create a subdomain results list
    results_list[[domain_name]] <- list()

    # iterate through each subset type
    for (subset_name in names(subset_types)) {
      # get the subset specification
      subset_spec <- subset_types[[subset_name]]

      # create result id
      result_id <- paste0("subset_batch_", subset_name, "_", domain_name)

      # output processing message
      message(paste0("Processing domain: ", domain_name, ", subset: ", subset_name))

      # perform batch processing
      batch_result <- margot_subset_batch(
        domain_models[[i]],
        X = X,
        base_defaults = base_defaults,
        subsets = subset_spec,
        original_df = original_df,
        interpret_all_E_gt1 = interpret_all_E_gt1
      )

      # save results
      here_save_qs(batch_result, result_id, push_mods)

      # view results
      cat(paste0("\n## Results for ", domain_name, " - ", subset_name, "\n"))
      print(batch_result$summary |> kbl("markdown"))

      # determine plot layout based on subset type
      plot_elements <- lapply(names(batch_result$results), function(group) {
        batch_result$results[[group]]$plot
      })

      # adjust ncol based on number of plots
      ncol_val <- if (length(plot_elements) > 2) 2 else 1

      # create and display plot
      combined_plot <- wrap_plots(
        plot_elements,
        ncol = ncol_val
      ) +
        patchwork::plot_annotation(
          title = subtitle,
          theme = theme(plot.title = element_text(size = 18, face = "bold"))
        )

      print(combined_plot)

      # explanation
      cat("\n## Explanation\n")
      cat(batch_result$explanation)

      # store results
      results_list[[domain_name]][[subset_name]] <- batch_result
    }
  }

  return(results_list)
}
