#' Policy Learning Methods Statement
#'
#' Generates a boilerplate methods paragraph describing the policy learning
#' workflow used for policy trees in `margot` (DR scores, tree fitting, honest
#' evaluation, policy value contrasts with bootstrap CIs, treated-only uplift,
#' and optional stability analysis).
#'
#' @param object A `margot_stability_policy_tree` (preferred) or a list with
#'   `$results` created by `margot_policy_tree()`.
#' @param depth Integer; tree depth described in text (default 2).
#' @param include_stability Logical; add details on the stability analysis if
#'   available (default TRUE).
#' @param include_policy_value Logical; mention policy value contrasts and CIs
#'   (default TRUE).
#' @param include_treated_only Logical; mention treated-only uplift and coverage
#'   (default FALSE).
#' @param citations Logical; include inline citations (author-year) (default TRUE).
#' @param style Character; one of "short" (1 paragraph) or "long" (multi-line)
#'   (default "short").
#' @return A character scalar with the methods paragraph(s).
#' @export
margot_policy_methods_statement <- function(object,
                                            depth = 2L,
                                            include_stability = TRUE,
                                            include_policy_value = TRUE,
                                            include_treated_only = FALSE,
                                            citations = TRUE,
                                            style = c("short", "long")) {
  style <- match.arg(style)

  md <- list()
  if (!is.null(object$metadata)) md <- object$metadata
  vary_type <- md$vary_type %||% NULL
  n_iter <- md$n_iterations %||% NULL
  tree_method <- md$tree_method %||% NULL
  train_props <- md$train_proportions %||% md$train_proportion %||% NULL
  metaseed <- md$metaseed %||% NULL

  cite <- function(txt) if (citations) paste0(" (", txt, ")") else ""
  meth <- if (!is.null(tree_method)) tree_method else "policytree"
  split_txt <- if (!is.null(train_props)) paste0("a held-out test fold (train proportion ", paste0(train_props, collapse = ","), ")") else "a held-out test fold"

  s1 <- paste0(
    "We estimated individualized treatment effects using doubly robust (DR) scores and learned depth-", depth,
    " policy trees to target treatment. DR scores were computed from causal forests and used as the policy learning objective",
    cite("Athey & Wager, 2021; grf; policytree"), ". Policy trees were fit with ", meth,
    ", and evaluated on ", split_txt, " to avoid adaptive overfitting (honest evaluation)."
  )

  s2 <- if (isTRUE(include_policy_value)) {
    paste0(
      "Policy value was summarized as the expected gain relative to universal control (policy − control-all) and relative to universal treatment (policy − treat-all), with 95% bootstrap confidence intervals."
    )
  } else ""

  s3 <- if (isTRUE(include_treated_only)) {
    paste0(
      "We also report the average uplift among units recommended for treatment (treated-only uplift) and the coverage (proportion recommended treatment), providing a concise link between expected impact and resource needs."
    )
  } else ""

  s4 <- if (isTRUE(include_stability) && inherits(object, "margot_stability_policy_tree")) {
    vt <- if (!is.null(vary_type)) switch(vary_type,
      split_only = "random train/test splits",
      bootstrap = "bootstrap resampling",
      both = "bootstrap resampling and train/test splits",
      vary_type
    ) else NULL
    nit <- if (!is.null(n_iter)) paste0(n_iter, " iterations of ") else ""
    seed <- if (!is.null(metaseed)) paste0(" (metaseed ", metaseed, ")") else ""
    paste0(
      "To assess robustness of tree structure, we conducted a stability analysis using ", nit, vt, seed,
      ". We report variable inclusion frequencies, consensus splits, and threshold variability to distinguish robust patterns from correlated proxies and sampling noise."
    )
  } else ""

  if (style == "short") paste(s1, s2, s3, s4) else paste(s1, s2, s3, s4, sep = "\n\n")
}
