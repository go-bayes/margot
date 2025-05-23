% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/margot_interpret_policy_tree.R
\name{margot_interpret_policy_tree}
\alias{margot_interpret_policy_tree}
\title{Interpret Policy Tree Results}
\usage{
margot_interpret_policy_tree(
  model,
  model_name,
  max_depth = 2L,
  train_proportion = 0.5,
  custom_action_names = NULL,
  label_mapping = NULL,
  original_df = NULL,
  remove_tx_prefix = TRUE,
  remove_z_suffix = TRUE,
  use_title_case = TRUE
)
}
\arguments{
\item{model}{A list containing the results from a multi-arm causal forest model.}

\item{model_name}{A string specifying which model's results to interpret.}

\item{max_depth}{Integer, 1 or 2; which stored tree to interpret.}

\item{train_proportion}{Numeric value between 0 and 1 for the proportion of data used for training. Default is 0.5.}

\item{custom_action_names}{Optional vector of custom names for the actions. Must match the number of actions in the policy tree.}

\item{label_mapping}{Optional list that maps variable names to custom labels.}

\item{original_df}{Optional dataframe with untransformed variables, used to display split values on the data scale.}

\item{remove_tx_prefix}{Logical indicating whether to remove prefixes like t0_ from variable names. Default is TRUE.}

\item{remove_z_suffix}{Logical indicating whether to remove the _z suffix from variable names. Default is TRUE.}

\item{use_title_case}{Logical indicating whether to convert variable names to title case. Default is TRUE.}
}
\value{
Invisibly returns a string containing the interpretation; also prints it to the console.
}
\description{
This function creates an interpretation of policy tree results from a causal forest or multi-arm causal forest model.
It generates a formatted description of the policy tree, including the main splits and recommended actions.
}
