% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_policy_tree.R
\name{margot_policy_tree}
\alias{margot_policy_tree}
\title{Generate Comprehensive Policy Tree Analysis}
\usage{
margot_policy_tree(mc_test, model_name, ...)
}
\arguments{
\item{mc_test}{A list containing the results from a multi-arm causal forest model.}

\item{model_name}{A string specifying which model's results to analyze.}

\item{...}{Additional arguments passed to margot_plot_policy_tree and margot_interpret_policy_tree functions.}
}
\value{
A list containing four elements:
  \item{policy_tree_plot}{A ggplot object representing the policy tree visualization.}
  \item{policy_tree_interpretation}{A string containing the interpretation of the policy tree.}
  \item{qini_plot}{A ggplot object representing the Qini curve.}
  \item{decision_tree_visualisation}{A grViz object (DiagrammeR) representing the decision tree structure.}
}
\description{
This function provides a comprehensive analysis of policy tree results from a causal forest or multi-arm causal forest model.
It generates a policy tree plot, an interpretation of the policy tree, a Qini curve plot, and a decision tree visualization.
}
\examples{
\dontrun{
results <- margot_policy_tree(mc_test, "model_t2_log_hours_exercise_z")
print(results$policy_tree_plot)
cat(results$policy_tree_interpretation)
print(results$qini_plot)
print(results$decision_tree_visualisation)
}

}
