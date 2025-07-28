# internal helper function for applying labels
#' @keywords internal
.apply_label <- function(var_name, label_mapping = NULL) {
  if (!is.null(label_mapping) && var_name %in% names(label_mapping)) {
    return(label_mapping[[var_name]])
  }
  # fall back to transform_var_name with silent mode
  suppressMessages(transform_var_name(var_name, label_mapping))
}

#' Assess Variable Correlations for Policy Tree Analysis
#' 
#' @description
#' Analyzes correlations among covariates used in policy tree analysis.
#' This helps identify groups of correlated variables that may substitute
#' for each other in tree splits, explaining apparent instability.
#' 
#' @param model_results List returned by margot_causal_forest() (not bootstrap results).
#'   Must have been run with save_data = TRUE to access covariate data.
#' @param model_name Character string specifying which model to analyze
#' @param correlation_threshold Numeric threshold for considering variables correlated (default 0.5)
#' @param method Correlation method: "pearson", "spearman", or "kendall" (default "pearson")
#' @param plot Logical: Create correlation heatmap (default TRUE)
#' @param label_mapping Optional named list mapping variable names to labels. If NULL,
#'   uses automatic transformation via transform_var_name()
#' 
#' @return List containing:
#' \itemize{
#'   \item correlation_matrix: Full correlation matrix (with labelled row/column names)
#'   \item high_correlations: Pairs of variables with |r| > threshold (with labels)
#'   \item correlation_clusters: Groups of intercorrelated variables (with labels)
#'   \item summary_text: Narrative summary of findings (using labels)
#' }
#' 
#' @export
#' @importFrom stats cor hclust cutree as.dist
#' @importFrom cli cli_alert_info cli_alert_warning
margot_assess_variable_correlation <- function(
  model_results,
  model_name,
  correlation_threshold = 0.5,
  method = c("pearson", "spearman", "kendall"),
  plot = TRUE,
  label_mapping = NULL
) {
  
  method <- match.arg(method)
  
  # handle both bootstrap results and causal forest results
  if (inherits(model_results, "margot_bootstrap_policy_tree")) {
    cli::cli_alert_warning("Bootstrap results detected. To assess correlations, please provide the original causal forest results.")
    cli::cli_alert_info("The bootstrap results don't contain the full covariate data needed for correlation analysis.")
    stop("Please use the original model_results from margot_causal_forest(), not bootstrap results")
  }
  
  # validate inputs
  if (!model_name %in% names(model_results$results)) {
    stop("Model '", model_name, "' not found in results")
  }
  
  # extract covariates - try multiple locations
  model_data <- model_results$results[[model_name]]
  covariates <- NULL
  
  # try different locations where covariates might be stored
  if (!is.null(model_data$covariates_used)) {
    covariates <- model_data$covariates_used
  } else if (!is.null(model_data$X)) {
    covariates <- model_data$X
  } else if (!is.null(model_results$covariates)) {
    # use the global covariates if available
    covariates <- model_results$covariates
  }
  
  if (is.null(covariates)) {
    # try to reconstruct from plot_data if available
    if (!is.null(model_data$plot_data) && !is.null(model_data$plot_data$X_test_full)) {
      cli::cli_alert_warning("Using test set covariates from plot_data. Results will be based on test set only.")
      covariates <- model_data$plot_data$X_test_full
    } else {
      stop("Could not find covariates for model ", model_name, 
           ". Make sure the causal forest was run with save_data = TRUE")
    }
  }
  
  # ensure we have a matrix
  if (!is.matrix(covariates)) {
    covariates <- as.matrix(covariates)
  }
  
  # compute correlation matrix
  cli::cli_alert_info("Computing {method} correlations among {ncol(covariates)} variables")
  cor_mat <- cor(covariates, method = method, use = "pairwise.complete.obs")
  
  # create labeled version of correlation matrix
  cor_mat_labeled <- cor_mat
  var_labels <- sapply(colnames(cor_mat), function(v) .apply_label(v, label_mapping))
  rownames(cor_mat_labeled) <- var_labels
  colnames(cor_mat_labeled) <- var_labels
  
  # identify high correlations
  high_cor_pairs <- list()
  cor_upper <- upper.tri(cor_mat)
  
  for (i in 1:(ncol(cor_mat)-1)) {
    for (j in (i+1):ncol(cor_mat)) {
      if (abs(cor_mat[i, j]) > correlation_threshold) {
        var1_name <- colnames(cor_mat)[i]
        var2_name <- colnames(cor_mat)[j]
        high_cor_pairs[[length(high_cor_pairs) + 1]] <- list(
          var1 = var1_name,
          var2 = var2_name,
          var1_label = .apply_label(var1_name, label_mapping),
          var2_label = .apply_label(var2_name, label_mapping),
          correlation = cor_mat[i, j]
        )
      }
    }
  }
  
  # identify correlation clusters using hierarchical clustering
  if (length(high_cor_pairs) > 0) {
    # create distance matrix from correlations
    cor_dist <- as.dist(1 - abs(cor_mat))
    hc <- hclust(cor_dist, method = "complete")
    
    # cut tree at height corresponding to correlation threshold
    clusters <- cutree(hc, h = 1 - correlation_threshold)
    
    # organize clusters with labels
    cluster_list <- list()
    cluster_list_labeled <- list()
    for (k in unique(clusters)) {
      vars_in_cluster <- names(clusters)[clusters == k]
      if (length(vars_in_cluster) > 1) {
        cluster_list[[paste0("cluster_", k)]] <- vars_in_cluster
        # create labeled version
        vars_labeled <- sapply(vars_in_cluster, function(v) .apply_label(v, label_mapping))
        cluster_list_labeled[[paste0("cluster_", k)]] <- vars_labeled
      }
    }
  } else {
    cluster_list <- list()
    cluster_list_labeled <- list()
  }
  
  # create summary text
  summary_text <- character()
  
  if (length(high_cor_pairs) == 0) {
    summary_text <- paste0(
      "No variable pairs exceeded the correlation threshold of ",
      correlation_threshold, ". This suggests that policy tree instability ",
      "is not primarily driven by multicollinearity among predictors."
    )
  } else {
    summary_text <- c(summary_text, paste0(
      "Found ", length(high_cor_pairs), " variable pairs with |r| > ",
      correlation_threshold, ". "
    ))
    
    if (length(cluster_list) > 0) {
      summary_text <- c(summary_text, paste0(
        "These form ", length(cluster_list), " clusters of intercorrelated variables. ",
        "Variables within each cluster may substitute for one another in tree splits, ",
        "contributing to apparent instability while capturing similar underlying information."
      ))
      
      # describe largest clusters using labels
      cluster_sizes <- sapply(cluster_list, length)
      largest_cluster_names <- names(cluster_list)[order(cluster_sizes, decreasing = TRUE)]
      largest_cluster_names <- head(largest_cluster_names, 3)
      
      for (i in seq_along(largest_cluster_names)) {
        cluster_name <- largest_cluster_names[i]
        cluster_vars_labeled <- cluster_list_labeled[[cluster_name]]
        summary_text <- c(summary_text, paste0(
          "\n\nCluster ", i, " contains: ",
          paste(cluster_vars_labeled, collapse = ", ")
        ))
      }
    }
  }
  
  # create plot if requested
  if (plot && requireNamespace("ggplot2", quietly = TRUE) && 
      requireNamespace("reshape2", quietly = TRUE)) {
    
    # prepare data for plotting with labeled correlation matrix
    cor_melt <- reshape2::melt(cor_mat_labeled)
    
    p <- ggplot2::ggplot(cor_melt, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                    midpoint = 0, limit = c(-1, 1),
                                    name = paste0(stringr::str_to_title(method), "\nCorrelation")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(title = paste0("Variable Correlations for ", .apply_label(model_name, label_mapping)),
                    x = "", y = "")
    
    print(p)
  }
  
  # return results
  structure(
    list(
      correlation_matrix = cor_mat,
      correlation_matrix_labeled = cor_mat_labeled,
      high_correlations = high_cor_pairs,
      correlation_clusters = cluster_list,
      correlation_clusters_labeled = cluster_list_labeled,
      summary_text = paste(summary_text, collapse = ""),
      model_name = model_name,
      model_name_label = .apply_label(model_name, label_mapping),
      threshold = correlation_threshold,
      method = method,
      label_mapping = label_mapping
    ),
    class = "margot_correlation_assessment"
  )
}

#' Print method for margot_correlation_assessment
#' @export
print.margot_correlation_assessment <- function(x, ...) {
  cat("Variable Correlation Assessment for", x$model_name_label, "\n")
  cat("Method:", x$method, "| Threshold:", x$threshold, "\n\n")
  cat(x$summary_text, "\n")
  invisible(x)
}

#' Identify Variable Clusters for Policy Tree Analysis
#' 
#' @description
#' Groups variables into clusters based on their correlations, helping
#' to understand which variables might substitute for each other in
#' policy tree splits.
#' 
#' @param correlation_assessment Output from margot_assess_variable_correlation()
#' @param min_cluster_size Minimum number of variables to form a cluster (default 2)
#' @param label_mapping Optional named list mapping variable names to labels. If NULL and
#'   correlation_assessment contains label_mapping, uses that; otherwise uses automatic transformation
#' 
#' @return Data frame with cluster assignments and representative variables (using labels)
#' 
#' @export
margot_identify_variable_clusters <- function(
  correlation_assessment,
  min_cluster_size = 2,
  label_mapping = NULL
) {
  
  if (!inherits(correlation_assessment, "margot_correlation_assessment")) {
    stop("Input must be output from margot_assess_variable_correlation()")
  }
  
  # use label_mapping from correlation_assessment if not provided
  if (is.null(label_mapping) && !is.null(correlation_assessment$label_mapping)) {
    label_mapping <- correlation_assessment$label_mapping
  }
  
  clusters <- correlation_assessment$correlation_clusters
  cor_mat <- correlation_assessment$correlation_matrix
  
  if (length(clusters) == 0) {
    cli::cli_alert_info("No variable clusters found at the specified correlation threshold")
    return(NULL)
  }
  
  # create cluster summary
  cluster_summary <- list()
  
  for (i in seq_along(clusters)) {
    cluster_vars <- clusters[[i]]
    
    if (length(cluster_vars) >= min_cluster_size) {
      # extract submatrix for this cluster
      cluster_cor <- cor_mat[cluster_vars, cluster_vars]
      
      # find most central variable (highest average correlation with others)
      avg_cor <- rowMeans(abs(cluster_cor))
      representative <- names(which.max(avg_cor))
      
      # compute within-cluster correlation
      within_cor <- mean(abs(cluster_cor[upper.tri(cluster_cor)]))
      
      # apply labels
      cluster_vars_labeled <- sapply(cluster_vars, function(v) .apply_label(v, label_mapping))
      representative_label <- .apply_label(representative, label_mapping)
      
      cluster_summary[[i]] <- data.frame(
        cluster_id = i,
        n_variables = length(cluster_vars),
        representative_variable = representative,
        representative_variable_label = representative_label,
        avg_within_correlation = within_cor,
        variables = paste(cluster_vars, collapse = ", "),
        variables_labeled = paste(cluster_vars_labeled, collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(cluster_summary) > 0) {
    result <- do.call(rbind, cluster_summary)
    
    # add interpretation
    cat("Variable Cluster Analysis\n")
    cat("=========================\n\n")
    
    for (i in 1:nrow(result)) {
      cat(paste0("Cluster ", result$cluster_id[i], ":\n"))
      cat(paste0("  Representative: ", result$representative_variable_label[i], "\n"))
      cat(paste0("  Size: ", result$n_variables[i], " variables\n"))
      cat(paste0("  Avg correlation: ", round(result$avg_within_correlation[i], 3), "\n"))
      cat(paste0("  Members: ", result$variables_labeled[i], "\n\n"))
    }
    
    cat("\nInterpretation: Variables within each cluster are highly correlated and may\n")
    cat("substitute for one another in policy tree splits. When interpreting bootstrap\n")
    cat("results, consider these clusters as representing similar underlying constructs.\n")
    
    return(result)
  } else {
    cli::cli_alert_info("No clusters with {min_cluster_size} or more variables found")
    return(NULL)
  }
}

#' Diagnose Policy Tree Stability
#' 
#' @description
#' Comprehensive stability diagnostics for policy trees, combining
#' correlation analysis with bootstrap results to provide actionable
#' insights about tree instability.
#' 
#' @param bootstrap_results Output from margot_policy_tree_bootstrap()
#' @param model_results Original causal forest results
#' @param model_name Model to analyze
#' @param correlation_threshold Threshold for correlation analysis (default 0.5)
#' 
#' @return List containing diagnostic results and recommendations
#' 
#' @export
margot_stability_diagnostics <- function(
  bootstrap_results,
  model_results,
  model_name,
  correlation_threshold = 0.5
) {
  
  # run correlation analysis
  cor_analysis <- margot_assess_variable_correlation(
    model_results = model_results,
    model_name = model_name,
    correlation_threshold = correlation_threshold,
    plot = FALSE
  )
  
  # get bootstrap metrics
  boot_metrics <- bootstrap_results$results[[model_name]]$bootstrap_metrics
  var_freq <- boot_metrics$var_inclusion_freq
  
  # identify frequently selected variables
  freq_vars_d1 <- var_freq[var_freq$depth_1_freq > 0.1, ]
  freq_vars_d1 <- freq_vars_d1[order(freq_vars_d1$depth_1_freq, decreasing = TRUE), ]
  
  # check if frequently selected variables are correlated
  if (nrow(freq_vars_d1) > 1 && length(cor_analysis$correlation_clusters) > 0) {
    # find which frequent variables belong to same clusters
    correlated_freq_vars <- list()
    
    for (cluster in cor_analysis$correlation_clusters) {
      cluster_freq_vars <- intersect(cluster, freq_vars_d1$variable)
      if (length(cluster_freq_vars) > 1) {
        correlated_freq_vars[[length(correlated_freq_vars) + 1]] <- cluster_freq_vars
      }
    }
    
    stability_interpretation <- paste0(
      "The bootstrap analysis shows that several frequently selected variables ",
      "belong to the same correlation clusters. This explains the apparent ",
      "instability: the trees are consistently identifying important subgroups, ",
      "but the specific variable chosen varies due to multicollinearity.\n\n",
      "Key findings:\n"
    )
    
    if (length(correlated_freq_vars) > 0) {
      for (i in seq_along(correlated_freq_vars)) {
        vars <- correlated_freq_vars[[i]]
        freqs <- freq_vars_d1$depth_1_freq[freq_vars_d1$variable %in% vars]
        stability_interpretation <- paste0(
          stability_interpretation,
          "- Correlated variables ", paste(vars, collapse = ", "),
          " were selected with frequencies ",
          paste(round(freqs * 100, 1), collapse = "%, "),
          "%\n"
        )
      }
    }
  } else {
    stability_interpretation <- paste0(
      "The frequently selected variables in bootstrap analysis do not show ",
      "strong correlations with each other. This suggests that instability ",
      "may be due to weak treatment effect heterogeneity rather than ",
      "multicollinearity among predictors."
    )
  }
  
  # provide recommendations
  recommendations <- character()
  
  if (length(cor_analysis$correlation_clusters) > 0) {
    recommendations <- c(recommendations,
      "1. Consider creating composite variables from correlated clusters",
      "2. Use domain knowledge to select representative variables from each cluster",
      "3. Report findings in terms of variable clusters rather than individual variables"
    )
  }
  
  if (boot_metrics$consensus_strength$depth_1 < 0.5) {
    recommendations <- c(recommendations,
      "4. Consider the trade-off between stability and performance - unstable depth-2 trees often outperform stable alternatives",
      "5. Collect additional data to better characterize heterogeneity",
      "6. Use the bootstrap results to quantify uncertainty in variable selection"
    )
  }
  
  # compile results
  diagnostics <- list(
    correlation_analysis = cor_analysis,
    bootstrap_metrics = boot_metrics,
    correlated_frequent_vars = if (exists("correlated_freq_vars")) correlated_freq_vars else NULL,
    stability_interpretation = stability_interpretation,
    recommendations = recommendations
  )
  
  class(diagnostics) <- "margot_stability_diagnostics"
  
  # print summary
  cat("Policy Tree Stability Diagnostics\n")
  cat("=================================\n\n")
  cat(stability_interpretation, "\n\n")
  
  if (length(recommendations) > 0) {
    cat("Recommendations:\n")
    cat(paste(recommendations, collapse = "\n"), "\n")
  }
  
  invisible(diagnostics)
}