# -----------------------------------------------------------------------------
# margot_simulate_refactor.R – experimental refactor (complete)
# -----------------------------------------------------------------------------
# author: chatgpt o3 (June 2025)
# -----------------------------------------------------------------------------
# Change log
# v1.0 2025-06-09 Complete function body; optional attrition indicator
#                  (default FALSE); per-wave column order L → A → C → Y.
# -----------------------------------------------------------------------------

# ---- default parameters -------------------------------------------------
#' Default simulation parameter values
#'
#' Helper returning the list of scalar coefficients that control the
#' attrition, exposure, and outcome models used by
#' \code{\link{margot_simulate}}.  Override any subset by supplying a
#' named list to the \code{params} argument of the simulator.
#'
#' @return A named \code{list}.
#' @keywords internal
#' @noRd
.default_sim_params <- function() {
  list(
    cens_exp_coef   = 0.4,  # prev A → logit(attrition)
    cens_latent_rho = 0.5,  # shared frailty SD
    exp_intercept   = -0.2, # baseline logit(A)
    exp_L1_coef     = 0.2,  # L1 → A
    out_B1_coef     = 0.1   # B1 → Y
  )
}

# ---- attrition helper ---------------------------------------------------
#' Linear predictor for the attrition process
#'
#' Internal helper that augments a baseline logit–scale linear predictor
#' with optional exposure‐ and latent‐frailty terms.
#'
#' @param base_lp Numeric vector of baseline linear predictors.
#' @param idx Integer indices of individuals still “alive” at the
#'   current wave.
#' @param prev_A Numeric vector giving previous‐wave exposure status.
#' @param cens_spec Named list with elements
#'   \code{exposure_dependence} (logical),
#'   \code{latent_dependence}  (logical), and
#'   \code{latent_rho} (numeric, SD of shared frailty).
#' @param params Named list of simulation coefficients as returned by
#'   \code{.default_sim_params()}.
#'
#' @return Numeric vector of the same length as \code{base_lp}.
#' @keywords internal
#' @noRd
calc_lp <- function(base_lp, idx, prev_A, cens_spec, params) {
  lp <- base_lp
  if (cens_spec$exposure_dependence)
    lp <- lp + params$cens_exp_coef * prev_A[idx]
  if (cens_spec$latent_dependence)
    lp <- lp + cens_spec$latent_rho * rnorm(length(idx))
  lp
}

# ---- simulator ----------------------------------------------------------
#' Simulate longitudinal exposures, outcomes, and covariates
#'
#' `margot_simulate()` draws baseline covariates (`B`), time-varying
#' covariates (`L`), exposures (`A`), and lead outcomes (`Y`) for a
#' synthetic panel study.  Monotone attrition can depend on past exposure
#' and/or a latent shared frailty, and an optional indicator column
#' records whether each unit remains uncensored at every wave.  The
#' simulator supports heterogeneous treatment effects, feedback from
#' previous outcomes into future exposures, and marginal item
#' missingness.
#'
#' @section The `params` argument:
#' Supply a named list to override the internal defaults given by
#' \code{.default_sim_params()}.  Typical entries include
#' \code{cens_exp_coef}, \code{exp_L1_coef}, and \code{out_B1_coef}.
#'
#' @param n                   Number of individuals.
#' @param waves               Number of follow-up waves (outcomes are
#'   produced for wave \code{waves + 1}).
#' @param exposures           Named list describing each exposure.  Every
#'   element must contain a \code{type} field (`"binary"` or `"normal"`).
#'   Optional elements: \code{het} (baseline modifiers) and
#'   \code{lag_Y = TRUE} to enable outcome-to-exposure feedback.
#' @param outcomes            Named list describing outcomes.  Defaults to a
#'   single normal outcome called `"Y"`.
#' @param p_covars            Number of baseline (`B`) covariates.
#' @param censoring           List controlling attrition.  Must include
#'   \code{rate}; optional logical flags \code{exposure_dependence},
#'   \code{latent_dependence}, numeric \code{latent_rho}, and logical
#'   \code{indicator} to append \code{tX_not_censored} columns.
#' @param item_missing_rate   MCAR probability an observed value is replaced
#'   by \code{NA}.
#' @param exposure_outcome    Coefficient for the exposure → outcome path.
#' @param y_feedback          Coefficient for lagged outcome feedback when
#'   an exposure lists \code{lag_Y = TRUE}.
#' @param positivity          `"good"`, `"poor"`, or a numeric probability
#'   in (0, 1) governing baseline exposure prevalence.
#' @param outcome_type        Shortcut for a single outcome:
#'   `"continuous"` (default) or `"binary"`.  Ignored when
#'   \code{outcomes} is supplied.
#' @param wide                If \code{TRUE} (default) return a wide data
#'   set; otherwise long format.
#' @param seed                Integer seed for reproducibility.
#' @param params              Named list of scalar coefficients (see above).
#' @param ...                 Deprecated arguments; ignored with a warning.
#'
#' @return A `tibble` in wide or long form containing baseline `B`
#'   variables, time-varying `L` covariates, `A` exposures, optional
#'   censoring indicators, and lead outcomes `Y`.  The object carries an
#'   attribute \code{"margot_meta"} with the matched call and a timestamp.
#'
#' @details
#' The default parameter set is
#' \preformatted{
#'  .default_sim_params()
#'  ## $cens_exp_coef   0.4
#'  ## $cens_latent_rho 0.5
#'  ## $exp_intercept   -0.2
#'  ## $exp_L1_coef     0.2
#'  ## $out_B1_coef     0.1
#' }
#'
#' @examples
#' ## basic usage
#' dat <- margot_simulate(n = 200, waves = 3, seed = 1)
#' dplyr::glimpse(dat)
#'
#' ## heterogeneous treatment effect with censoring indicator
#' dat2 <- margot_simulate(
#'   n = 800,
#'   waves = 4,
#'   exposures = list(
#'     A1 = list(
#'       type = "binary",
#'       het  = list(modifier = "B2", coef = 0.6)
#'     )
#'   ),
#'   censoring = list(rate = 0.25, exposure_dependence = TRUE,
#'                    indicator = TRUE),
#'   seed = 42
#' )
#'
#' @import dplyr tibble MASS
#' @importFrom stats rnorm rbinom qlogis plogis pnorm
#' @seealso \code{\link{.default_sim_params}}
#' @export
margot_simulate <- function(
    n,
    waves,
    exposures          = NULL,
    outcomes           = NULL,
    p_covars           = 20,
    censoring          = list(rate = 0.25),
    item_missing_rate  = 0,
    exposure_outcome   = 0.5,
    y_feedback         = 0.4,
    positivity         = "good",
    outcome_type       = NULL,
    wide               = TRUE,
    seed               = NULL,
    params             = list(),
    ...) {

  # -- handle deprecated dots --------------------------------------------
  dots <- list(...)
  if ("covar_feedback" %in% names(dots)) {
    warning("`covar_feedback` is deprecated; use params$exp_L1_coef instead.")
    params$exp_L1_coef <- dots$covar_feedback
  }
  if ("jitter_censoring" %in% names(dots))
    warning("`jitter_censoring` no longer has any effect and is ignored.")

  if (!is.null(seed)) set.seed(seed)

  # -- merge parameter lists ---------------------------------------------
  params    <- modifyList(.default_sim_params(), params)
  censoring <- modifyList(list(rate = 0.25,
                               exposure_dependence = FALSE,
                               latent_dependence   = FALSE,
                               latent_rho          = params$cens_latent_rho,
                               indicator           = FALSE),  # optional C cols
                          censoring)

  # -- default exposure / outcome specs ----------------------------------
  if (is.null(exposures)) exposures <- list(A1 = list(type = "binary"))
  if (is.null(outcomes)) {
    if (is.null(outcome_type) || outcome_type[1] == "continuous") {
      outcomes <- list(Y = list(type = "normal"))
    } else if (outcome_type[1] == "binary") {
      outcomes <- list(Y = list(type = "binary", p = 0.5))
    } else stop("Unknown outcome_type")
  }
  k_out <- length(outcomes)

  # -- helpers ------------------------------------------------------------
  rbern <- function(x, p = NULL) {
    prob <- if (is.null(p)) x else if (length(p) == 1) rep(p, x) else p
    prob[is.na(prob) | prob < 0] <- 0; prob[prob > 1] <- 1
    stats::rbinom(length(prob), 1, prob)
  }
  sprinkle <- function(v) {
    if (item_missing_rate == 0) return(v)
    v[runif(length(v)) < item_missing_rate] <- NA
    v
  }

  # -- baseline positivity prob ------------------------------------------
  pos_prob <- switch(as.character(positivity[1]),
                     good = 0.5,
                     poor = 0.9,
                     as.numeric(positivity[1]))
  if (is.na(pos_prob) || pos_prob <= 0 || pos_prob >= 1) pos_prob <- 0.5

  # -- draw baseline covariates + initial A ------------------------------
  Sigma_B <- matrix(0.3, p_covars, p_covars); diag(Sigma_B) <- 1
  df <- tibble::tibble(id = seq_len(n)) |>
    dplyr::bind_cols(
      tibble::as_tibble(MASS::mvrnorm(n, rep(0, p_covars), Sigma_B),
                        .name_repair = ~ paste0("B", seq_along(.x)))
    )
  for (ex in names(exposures)) {
    df[[paste0("t0_", ex)]] <- if (exposures[[ex]]$type == "binary")
      rbern(n, pos_prob) else rnorm(n)
  }

  # -- trackers -----------------------------------------------------------
  alive     <- rep(TRUE, n)
  last_Y    <- matrix(0, n, k_out)  # for optional lag-Y feedback
  long_list <- if (!wide) vector("list", waves) else NULL

  # -- main follow-up loop -----------------------------------------------
  for (t in seq_len(waves)) {
    tag      <- paste0("t", t)
    prev_tag <- if (t == 1) "t0" else paste0("t", t - 1)

    # --- attrition -------------------------------------------------------
    lp0       <- rep(-Inf, n)
    idx_alive <- which(alive)
    if (length(idx_alive)) {
      lp0[idx_alive] <- qlogis(1 - censoring$rate)
      prev_A         <- df[[paste0(prev_tag, "_", names(exposures)[1])]]
      lp0[idx_alive] <- calc_lp(lp0[idx_alive], idx_alive, prev_A, censoring, params)
    }
    C_wave <- rbern(plogis(lp0))
    alive  <- alive & as.logical(C_wave)

    nc_df <- if (censoring$indicator) {
      tibble::tibble(!!paste0(tag, "_not_censored") := as.integer(alive))
    } else NULL

    # --- time-varying covariates L --------------------------------------
    L_df <- tibble::tibble(.rows = n)
    idx  <- which(alive)
    for (j in 1:3) {
      v <- rep(NA_real_, n); if (length(idx)) v[idx] <- rnorm(length(idx))
      L_df[[paste0(tag, "_L", j)]] <- sprinkle(v)
    }

    # --- exposures A -----------------------------------------------------
    A_df  <- tibble::tibble(.rows = n); A_cols <- list()
    for (ex in names(exposures)) {
      spec <- exposures[[ex]]
      if (isTRUE(spec$lag_Y) && is.null(spec$lag_coef)) spec$lag_coef <- y_feedback
      v <- rep(NA_real_, n)
      if (length(idx)) {
        if (spec$type == "binary") {
          lpA <- params$exp_intercept + params$exp_L1_coef * L_df[[paste0(tag, "_L1")]][idx]
          if (isTRUE(spec$lag_Y)) lpA <- lpA + spec$lag_coef * last_Y[idx, 1]
          v[idx] <- rbern(plogis(lpA))
        } else {
          v[idx] <- rnorm(length(idx), mean = params$exp_L1_coef * L_df[[paste0(tag, "_L1")]][idx])
        }
      }
      A_cols[[ex]] <- v
      A_df[[paste0(tag, "_", ex)]] <- sprinkle(v)
    }

    # --- lead-1 outcome block (final wave only) -------------------------
    Y_df <- tibble::tibble(.rows = n)
    if (t == waves) {
      lpY <- rep(-Inf, n)
      idx_alive2 <- which(alive)
      if (length(idx_alive2)) {
        lpY[idx_alive2] <- qlogis(1 - censoring$rate)
        lpY[idx_alive2] <- calc_lp(lpY[idx_alive2], idx_alive2, A_cols[[1]], censoring, params)
      }
      keep_Y  <- rbern(plogis(lpY))
      alive_Y <- alive & as.logical(keep_Y)

      idx_Y <- which(alive_Y)
      Err <- if (length(idx_Y))
        MASS::mvrnorm(length(idx_Y), rep(0, k_out), diag(k_out))
      else
        matrix(NA_real_, 0, k_out)
      Err <- matrix(Err, nrow = length(idx_Y), ncol = k_out)

      for (k in seq_along(outcomes)) {
        spec <- outcomes[[k]]
        y <- rep(NA_real_, n)
        if (length(idx_Y)) {
          mu <- exposure_outcome * A_cols[[1]][idx_Y] +
            params$out_B1_coef * df$B1[idx_Y]

          if (!is.null(exposures[[1]]$het)) {
            gamma  <- exposures[[1]]$het$coef
            modcol <- exposures[[1]]$het$modifier
            mu <- mu + gamma * A_cols[[1]][idx_Y] * df[[modcol]][idx_Y]
          }

          if (spec$type == "normal") {
            y[idx_Y] <- mu + Err[, k]
          } else {                    # binary outcome
            lp <- -1 + mu
            y[idx_Y] <- rbern(pnorm(lp + Err[, k]))
          }
        }
        Y_df[[paste0("t", t + 1, "_", names(outcomes)[k])]] <- sprinkle(y)
        last_Y[, k] <- y
      }  # end outcome loop
    }    # end if final wave

    # --- bind wave-level blocks ----------------------------------------
    blocks <- list(L_df, A_df, Y_df)
    if (!is.null(nc_df)) blocks <- append(blocks, list(nc_df), 2)
    df <- dplyr::bind_cols(df, !!!blocks)

    if (!wide) long_list[[t]] <- dplyr::bind_cols(id = df$id, wave = t, !!!blocks)
  }         # end follow-up loop

  # -- baseline MCAR -----------------------------------------------------
  if (item_missing_rate > 0) {
    bcols <- grep("^B", names(df), value = TRUE)
    df[bcols] <- lapply(df[bcols], sprinkle)
  }

  # -- attach metadata ---------------------------------------------------
  attr(df, "margot_meta") <- list(args = match.call(), timestamp = Sys.time())

  # -- return wide or long ----------------------------------------------
  if (wide) return(df)

  base_block <- df |>
    dplyr::select(id, dplyr::starts_with("t0_"), dplyr::starts_with("B")) |>
    dplyr::mutate(wave = 0) |>
    dplyr::relocate(wave, .after = id)

  long_out <- if (length(long_list)) dplyr::bind_rows(long_list) else NULL
  out_long <- dplyr::bind_rows(base_block, long_out)
  attr(out_long, "margot_meta") <- attr(df, "margot_meta")
  out_long
}
