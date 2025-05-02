#' Post-process models to recover Qini curves via propensity trimming
#'
#' @description
#' for any model in a results list whose qini_objects are null or empty,
#' this function applies an overlap restriction on the estimated\ n#' propensity scores and recomputes in‑sample Qini curves on the trimmed data,
#' without touching the original ATE or forest objects.
#'
#' @param model_results a list from margot_causal_forest() with
#'   save_models = TRUE and save_data = TRUE
#' @param propensity_bounds numeric length‑2 vector of lower/upper
#'   bounds for forest$W.hat (default c(0.05, 0.95))
#' @param verbose logical; if TRUE prints progress messages (default TRUE)
#'
#' @return modified model_results with rescued qini_data and qini_objects
#'   for any models that initially had empty gain
#' @keywords internal
margot_rescue_qini <- function(model_results,
                               propensity_bounds = c(0.05, 0.95),
                               verbose = TRUE) {

  stopifnot(is.list(model_results),
            !is.null(model_results$results),
            !is.null(model_results$full_models),
            !is.null(model_results$data))

  for (mn in names(model_results$results)) {
    mr <- model_results$results[[mn]]

    # skip if we already have non‑empty qini_data
    if (!is.null(mr$qini_data) && NROW(mr$qini_data) > 0) next

    if (verbose) message("rescuing qini for ", mn)

    forest <- model_results$full_models[[mn]]
    outcome <- sub("^model_", "", mn)
    Y   <- model_results$data[[outcome]]
    W   <- forest$W
    eh  <- forest$W.hat
    tau <- mr$tau_hat

    # apply overlap trimming
    keep <- which(eh > propensity_bounds[1] &
                    eh < propensity_bounds[2] &
                    !is.na(Y))

    if (length(keep) < 10) {
      if (verbose) message("  too few obs after trimming; skipping")
      next
    }

    # recompute in‐sample qini on trimmed set
    rescue <- compute_qini_curves_binary(
      tau[keep], Y[keep], W[keep], verbose = FALSE
    )

    if (is.null(rescue)) {
      if (verbose) message("  qini recompute returned NULL")
      next
    }

    # save rescued results
    mr$qini_data    <- rescue$qini_data
    mr$qini_objects <- rescue$qini_objects
    model_results$results[[mn]] <- mr
  }

  return(model_results)
}
