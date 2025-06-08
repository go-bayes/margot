#' Simulate Longitudinal Panel Data for Causal Inference
#'
#' Generates synthetic longitudinal panel data with flexible options for treatment
#' assignment, outcome generation, censoring, and missing data patterns. Useful for
#' testing causal inference methods and teaching.
#'
#' @param n Integer. Number of individuals (default: 500).
#' @param waves Integer. Number of measurement waves (1-20, default: 3).
#' @param p_covars Integer. Number of time-varying covariates (default: 1).
#' @param censoring List with components:
#'   \describe{
#'     \item{rate}{Baseline censoring rate (default: 0.2)}
#'     \item{exposure_dependence}{Logical. Does censoring depend on prior exposure? (default: FALSE)}
#'     \item{latent_dependence}{Logical. Does censoring depend on latent factor? (default: FALSE)}
#'     \item{latent_rho}{Correlation with latent factor if latent_dependence = TRUE (default: 0.5)}
#'   }
#' @param item_missing_rate Numeric. MCAR rate for covariates (0-1, default: 0).
#' @param positivity Character. Positivity assumption scenario:
#'   \describe{
#'     \item{"good"}{Well-behaved propensity scores}
#'     \item{"edge"}{Near-violation of positivity}
#'     \item{"violated"}{Deterministic treatment assignment}
#'   }
#' @param exposure_outcome Numeric. Effect of treatment on outcome (default: 0.6).
#' @param covar_feedback Numeric. Effect of treatment on future covariates (default: 0).
#' @param y_feedback Numeric. Effect of previous outcome on next treatment (default: 0).
#' @param outcome_type Character. Type of outcome variable ("binary" or "continuous").
#' @param wide Logical. Return wide format if TRUE, long format if FALSE (default: FALSE).
#' @param seed Integer. Random seed for reproducibility (default: NULL).
#'
#' @return A tibble with simulated data. Format depends on \code{wide} parameter:
#'   \describe{
#'     \item{Wide format}{Columns: id, t0_l*, t0_y, t0_a, t1_l*, t1_y, t1_a, ..., y}
#'     \item{Long format}{Columns: id, wave, l1...lp, y, a, y_end}
#'   }
#'   
#'   In long format, \code{y_end} contains the final outcome (non-NA only at the 
#'   last observed wave for each individual).
#'
#' @details
#' The simulation generates:
#' \itemize{
#'   \item A latent baseline factor \code{u} affecting treatment and outcomes
#'   \item Time-varying covariates with optional AR(1) structure
#'   \item Treatment assignment based on covariates and latent factor
#'   \item Outcomes influenced by treatment, covariates, and latent factor
#'   \item Censoring patterns (MCAR or dependent on covariates/treatment)
#' }
#'
#' @examples
#' # basic simulation with default settings
#' dat <- margot_simulate(n = 200, waves = 3, seed = 123)
#' 
#' # simulate with treatment feedback and edge positivity
#' dat_complex <- margot_simulate(
#'   n = 500, 
#'   waves = 4,
#'   p_covars = 2,
#'   positivity = "edge",
#'   y_feedback = 0.5,
#'   covar_feedback = 0.3,
#'   wide = TRUE,
#'   seed = 2025
#' )
#' 
#' # simulate with censoring dependent on treatment
#' dat_censor <- margot_simulate(
#'   n = 300,
#'   waves = 5,
#'   censoring = list(
#'     rate = 0.3,
#'     exposure_dependence = TRUE,
#'     latent_dependence = TRUE,
#'     latent_rho = 0.7
#'   ),
#'   seed = 999
#' )
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols group_by ungroup cur_group_id mutate left_join all_of if_else
#' @importFrom tidyr pivot_wider
#' @importFrom stats rnorm rbinom plogis qlogis
#' @importFrom utils modifyList
margot_simulate <- function(
    n = 500,
    waves = 3,
    p_covars = 1,
    censoring = list(rate = 0.2,
                     exposure_dependence = FALSE,
                     latent_dependence   = FALSE,
                     latent_rho          = 0.5),
    item_missing_rate = 0,
    positivity        = c("good", "edge", "violated"),
    exposure_outcome  = 0.6,
    covar_feedback    = 0,
    y_feedback        = 0,
    outcome_type      = c("binary", "continuous"),
    wide              = FALSE,
    seed              = NULL
) {
  # input validation
  stopifnot(n > 0, waves >= 1, waves <= 20, p_covars >= 1,
            item_missing_rate >= 0, item_missing_rate <= 1)
  positivity   <- match.arg(positivity)
  outcome_type <- match.arg(outcome_type)
  if (!is.logical(wide)) stop("wide must be logical.")
  censoring <- modifyList(list(rate = 0.2, exposure_dependence = FALSE,
                               latent_dependence = FALSE, latent_rho = 0.5),
                          censoring)
  if (!is.null(seed)) set.seed(seed)
  y_feedback <- as.numeric(y_feedback)
  
  # latent baseline factor ------------------------------------------------
  u <- rnorm(n)
  
  # storage arrays --------------------------------------------------------
  l_arr <- array(NA_real_, c(n, p_covars, waves))
  a_mat <- matrix(NA_integer_, n, waves)
  y_mat <- matrix(NA_real_,    n, waves)
  
  # coefficients ----------------------------------------------------------
  treat_coef <- switch(positivity,
                       good     = list(int = -0.4, l = 0.6, u = 0.4, y = y_feedback),
                       edge     = list(int = -1.2, l = 1.2, u = 0.8, y = y_feedback),
                       violated = list(int = NA,   l = NA,  u = NA,  y = y_feedback))
  beta_y <- list(int = -0.3, a = exposure_outcome,
                 l = rep(0.3, p_covars), u = 0.4)
  delta_u <- if (censoring$latent_dependence) censoring$latent_rho else 0
  
  for (i in seq_len(n)) {
    censored <- FALSE
    
    # baseline covariates -------------------------------------------------
    l_arr[i, , 1] <- rnorm(p_covars)
    l_mean <- mean(l_arr[i, , 1])
    
    # baseline treatment --------------------------------------------------
    a_mat[i, 1] <- if (positivity == "violated") as.integer(l_mean > 0.5) else
      rbinom(1, 1, plogis(treat_coef$int + treat_coef$l * l_mean + treat_coef$u * u[i]))
    
    # baseline outcome ----------------------------------------------------
    mu_y1 <- beta_y$int + beta_y$a * a_mat[i, 1] +
      sum(beta_y$l * l_arr[i, , 1]) + beta_y$u * u[i]
    y_mat[i, 1] <- if (outcome_type == "binary") rbinom(1, 1, plogis(mu_y1)) else mu_y1 + rnorm(1)
    
    # baseline censoring --------------------------------------------------
    censored <- rbinom(1, 1, plogis(qlogis(censoring$rate) + delta_u * u[i])) == 1
    if (censored) {
      if (waves > 1) l_arr[i, , 2:waves] <- a_mat[i, 2:waves] <- y_mat[i, 2:waves] <- NA
      next
    }
    
    # subsequent waves ----------------------------------------------------
    if (waves > 1) for (t in 2:waves) {
      # covariates with treatment feedback
      for (j in seq_len(p_covars)) {
        l_arr[i, j, t] <- 0.5 * l_arr[i, j, t - 1] + covar_feedback * a_mat[i, t - 1] + rnorm(1)
      }
      if (item_missing_rate > 0) l_arr[i, , t][rbinom(p_covars, 1, item_missing_rate) == 1] <- NA
      l_mean_t <- mean(l_arr[i, , t], na.rm = TRUE); if (is.nan(l_mean_t)) l_mean_t <- 0
      
      # treatment ---------------------------------------------------------
      if (positivity == "violated") {
        a_mat[i, t] <- as.integer(l_mean_t > 0.5)
      } else {
        eta <- treat_coef$int + treat_coef$l * l_mean_t + treat_coef$u * u[i] + treat_coef$y * y_mat[i, t - 1]
        a_mat[i, t] <- rbinom(1, 1, plogis(eta))
      }
      
      # outcome -----------------------------------------------------------
      mu_yt <- beta_y$int + beta_y$a * a_mat[i, t] +
        sum(beta_y$l * l_arr[i, , t], na.rm = TRUE) + beta_y$u * u[i]
      y_mat[i, t] <- if (outcome_type == "binary") rbinom(1, 1, plogis(mu_yt)) else mu_yt + rnorm(1)
      
      # censoring ---------------------------------------------------------
      censored <- rbinom(1, 1, plogis(qlogis(censoring$rate) +
                                        if (censoring$exposure_dependence) 0.8 * a_mat[i, t - 1] else 0 +
                                        delta_u * u[i])) == 1
      if (censored) {
        if (t < waves) l_arr[i, , (t + 1):waves] <- a_mat[i, (t + 1):waves] <- y_mat[i, (t + 1):waves] <- NA
        break
      }
    }
  }
  
  final_y <- y_mat[, waves]
  
  # long ------------------------------------------------------------------
  id_vec   <- rep(seq_len(n), each = waves)
  wave_vec <- rep(seq_len(waves), times = n)
  l_df <- tibble(id = id_vec, wave = wave_vec)
  for (j in seq_len(p_covars)) l_df[[paste0("l", j)]] <- as.numeric(l_arr[, j, ])
  long_tbl <- bind_cols(l_df, tibble(y = as.numeric(y_mat), a = as.integer(a_mat))) %>%
    group_by(id) %>%
    mutate(y_end = if_else(wave == waves, final_y[cur_group_id()], NA_real_)) %>%
    ungroup()
  if (!wide) return(long_tbl)
  
  # wide ------------------------------------------------------------------
  pivot_cols <- c(paste0("l", seq_len(p_covars)), "y", "a")
  wide_tbl <- long_tbl %>% mutate(w0 = wave - 1) %>%
    pivot_wider(id_cols = id,
                names_from = w0,
                values_from = all_of(pivot_cols),
                names_glue = "t{w0}_{.value}") %>%
    left_join(tibble(id = seq_len(n), y = final_y), by = "id")
  
  col_blocks <- unlist(lapply(0:(waves - 1), function(k) {
    c(paste0("t", k, "_l", seq_len(p_covars)), paste0("t", k, "_y"), paste0("t", k, "_a"))
  }))
  ordered <- c("id", col_blocks, "y")
  wide_tbl <- wide_tbl[, intersect(ordered, names(wide_tbl))]
  attr(wide_tbl, "y_feedback") <- y_feedback
  wide_tbl
}