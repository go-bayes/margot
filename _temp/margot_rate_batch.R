# margot_rate_batch <- function(model_results,
#                               policy       = c("treat_best", "withhold_best"),
#                               target       = c("AUTOC", "QINI"),
#                               level        = 0.95,
#                               round_digits = 3,
#                               model_prefix = "model_",
#                               verbose      = TRUE) {
#
#   stopifnot(is.list(model_results),
#             all(c("results", "full_models") %in% names(model_results)))
#
#   policy <- match.arg(policy)
#   target <- match.arg(target)
#   z      <- qnorm(1 - (1 - level) / 2)
#
#   log_msg <- function(fun, msg) if (verbose) fun(msg)
#
#   # loop --------------------------------------------------------------------
#   out <- lapply(names(model_results$results), function(mn) {
#
#     forest  <- model_results$full_models[[mn]]
#     tau_hat <- model_results$results[[mn]]$tau_hat
#
#     if (is.null(forest) || !inherits(forest, "causal_forest")) {
#       log_msg(cli::cli_alert_warning,
#               sprintf("skip %s: no causal_forest", mn))
#       return(NULL)
#     }
#     if (!is.numeric(tau_hat)) {
#       log_msg(cli::cli_alert_warning,
#               sprintf("skip %s: tau_hat missing", mn))
#       return(NULL)
#     }
#
#     if (policy == "withhold_best") tau_hat <- -tau_hat
#
#     ra <- grf::rank_average_treatment_effect(forest, tau_hat, target = target)
#
#     se  <- ra$std.err
#     est <- ra$estimate
#     tibble::tibble(
#       model          = mn,
#       outcome        = sub(paste0("^", model_prefix), "", mn),
#       policy         = policy,
#       target         = target,
#       `RATE Estimate`= round(est, round_digits),
#       `Std Error`    = round(se,  round_digits),
#       `2.5%`         = round(est - z * se, round_digits),
#       `97.5%`        = round(est + z * se, round_digits)
#     )
#   })
#
#   dplyr::bind_rows(out)
# }
