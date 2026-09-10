# draw a constructed rule at the same width with legacy and compact layouts.
library(margot)
set.seed(20260910)
tree <- policytree::policy_tree(
  data.frame(score = c(1, 3, 4, 7)),
  cbind(control = 0, treated = c(1, 1, -1, -1)),
  depth = 1, min.node.size = 1
)
labels <- list(score = "Baseline score", control = "Assign g0", treated = "Assign g1")
legacy <- margot_plot_policy_decision_tree(
  tree, title = "Constructed policy rule", text_size = 3.2,
  label_mapping = labels
)
compact <- margot_plot_policy_decision_tree(
  tree, title = "Constructed policy rule", text_size = 3.2,
  label_mapping = labels, layout_style = "compact", branch_labels = "condition"
)
args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1]] else "man/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(file.path(output_dir, "policy-tree-layout-legacy.png"), legacy,
  width = 6.8, height = 3.4, dpi = 160, bg = "white")
ggplot2::ggsave(file.path(output_dir, "policy-tree-layout-compact.png"), compact,
  width = 6.8, height = 2, dpi = 160, bg = "white")
