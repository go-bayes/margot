#' Interpret LMTP positivity via effective sample sizes
#'
#' Builds a concise textual summary of LMTP density-ratio diagnostics for a
#' single outcome. For each requested shift, the function computes effective
#' sample sizes (ESS) by wave and for the pooled person-time using
#' `colSums()` over the underlying density ratios. The output mirrors the
#' filtering and labelling conventions used by
#' [`margot_plot_lmtp_overlap_grid()`] for consistency between prose and
#' graphics.
#'
#' @param x LMTP run output (e.g., the result of [margot_lmtp()]) or any object
#'   that exposes `$density_ratios` in the same structure as the plot helpers.
#' @param outcome Character scalar giving the outcome name to summarise.
#' @param shifts Optional character vector of shifts to include (either full
#'   names such as `t5_pwi_z_shift_up` or cleaned suffixes such as `shift_up`).
#'   If `NULL`, all available shifts for the outcome are used.
#' @param label_mapping Optional named list passed to [transform_label()] for
#'   readable outcome/shift labels.
#' @param waves Optional integer vector of wave indices to keep (matching the
#'   column positions used by the overlap plot). Defaults to all available
#'   waves.
#' @param remove_waves Optional integer vector of waves to exclude after any
#'   inclusion via `waves`.
#' @param digits Integer number of decimal places to use when reporting
#'   ESS-based fractions (e.g., `ESS/N`).
#' @param trim_right Numeric in (0, 1]; optional per-wave winsorisation level
#'   applied before forming cumulative density-ratio products (default 0.999).
#' @param include_methods Logical; if TRUE, prepends a methodological explanation of
#'   density ratios, their interpretation, and ESS computation (default: FALSE).
#' @param include_diagnostics Logical; if TRUE, appends detailed diagnostics per shift
#'   including zeros, range, quantiles, and tail probabilities (default: FALSE).
#' @param include_ipsi_context Logical; if TRUE and any `ipsi_*` shifts are
#'   included, prepends a short IPSI context block that explains the policy on the
#'   probability (risk) scale with a simple formula and small illustrative translations
#'   (default: TRUE).
#' @param treatment_label Character label used for $A_t$ in the IPSI context block.
#'   If NULL, attempts to infer a domain-appropriate label from model/shift names
#'   (e.g., "attendance" when shift/outcome names contain religious service
#'   keywords); otherwise falls back to "exposure". Default: NULL.
#' @param ipsi_example_g Numeric vector of example baseline risks g used to
#'   illustrate the transformation q = delta * g / ((1 - g) + delta * g) for the
#'   included delta values (default: `c(0.05, 0.10, 0.20)`).
#' @param include_policy_rates Logical; if TRUE and exposure-by-wave data are
#'   available and aligned with the density ratios, reports policy-implied exposure
#'   probabilities by wave using the reweighted mean p_hat_t = sum(r_{i,t} * A_{i,t}) / sum(r_{i,t})
#'   on uncensored rows (default: TRUE). If required inputs are missing, silently
#'   skips this section.
#' @param policy_rate_threshold Numeric; when computing policy rates and the attached
#'   exposure columns are counts or continuous, converts them to a binary indicator
#'   1(A_t op tau) before aggregation. Default tau = 0 (i.e., any exposure).
#' @param policy_rate_strict Logical; comparison operator used with the threshold when building
#'   the indicator: if TRUE uses '>' (strict), else uses '>=' (inclusive). Default TRUE.
#' @param include_deterministic_context Logical; if TRUE and any deterministic
#'   shifts are present (e.g., names starting with `shift_`), prepends a concise
#'   description of history‑dependent policies (e.g., A_t^d := d_t(A_t, H_t)) and,
#'   when possible, lists the named deterministic policies included (default: TRUE).
#' @param include_tests Logical; if TRUE, runs extra overlap/positivity checks and prints
#'   CLI messages summarising results (near‑zero uniform weights, product‑of‑r collapse,
#'   monotone support checks). Default TRUE.
#' @param test_thresholds Named list of thresholds for `include_tests`. Recognised names:
#'   `near_zero_median` (default 1e-3), `near_zero_cv` (0.05), `prod_log10` (-8),
#'   and `prod_frac_warn` (0.20). Unrecognised entries are ignored.
#' @param include_ipsi_recommend Logical; if TRUE and IPSI shifts are present, evaluates candidate
#'   deltas using the same tests and prints a recommendation for the largest delta that passes
#'   guardrails. Also adds an "IPSI Recommendation" section to the returned text. Default TRUE.
#' @param include_test_explanations Logical; if TRUE, adds a short "Test Explanations" section that
#'   explains near‑zero flags, the product‑of‑r summary, and how censoring is handled via IPCW. Default FALSE.
#' @param include_tests Logical; if TRUE, runs extra overlap/positivity checks and prints
#'   CLI messages summarising results (near‑zero uniform weights, product‑of‑r collapse,
#'   monotone support checks). Default TRUE.
#' @param test_thresholds Named list of thresholds for `include_tests`. Recognised names:
#'   `near_zero_median` (default 1e-3), `near_zero_cv` (0.05), `prod_log10` (-8),
#'   and `prod_frac_warn` (0.20). Unrecognised entries are ignored.
#' @param return Character; either `"text"` (default, a single markdown-ready
#'   string) or `"list"` (detailed components including the computed ESS
#'   tables).
#'
#' @return Either a single character string (default) or a list containing the
#'   header, per-shift lines, overview text, and the underlying ESS summaries
#'   when `return = "list"`.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' txt <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping
#' )
#' cat(txt)
#'
#' # With methodological explanation
#' txt_methods <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping,
#'   include_methods = TRUE
#' )
#' cat(txt_methods)
#'
#' # With detailed diagnostics
#' txt_diagnostics <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping,
#'   include_diagnostics = TRUE
#' )
#' cat(txt_diagnostics)
#'
#' # Complete report with methods and diagnostics
#' txt_full <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   label_mapping = label_mapping,
#'   include_methods = TRUE,
#'   include_diagnostics = TRUE
#' )
#' cat(txt_full)
#'
#' # Get structured list output
#' result <- margot_interpret_lmtp_positivity(
#'   fit,
#'   outcome = "t5_pwi_z",
#'   shifts = c("shift_up", "shift_down", "null"),
#'   include_methods = TRUE,
#'   include_diagnostics = TRUE,
#'   return = "list"
#' )
#' # Access components: result$methods, result$diagnostics, result$shifts
#' }
#'
#' @export
margot_interpret_lmtp_positivity <- function(x,
                                             outcome,
                                             shifts = NULL,
                                             label_mapping = NULL,
                                             waves = NULL,
                                             remove_waves = NULL,
                                             digits = 2,
                                             trim_right = 0.999,
                                             include_methods = FALSE,
                                             include_diagnostics = FALSE,
                                             include_ipsi_context = TRUE,
                                             treatment_label = NULL,
                                             ipsi_example_g = NULL,
                                             include_policy_rates = TRUE,
                                             policy_rate_threshold = 0,
                                             policy_rate_strict = TRUE,
                                             include_deterministic_context = TRUE,
                                             include_tests = TRUE,
                                             test_thresholds = NULL,
                                             include_ipsi_recommend = TRUE,
                                             include_test_explanations = FALSE,
                                             return = c("text", "list")) {
  stopifnot(is.character(outcome), length(outcome) == 1L)
  if (!is.null(shifts)) stopifnot(is.character(shifts))
  if (!is.null(waves)) stopifnot(is.numeric(waves))
  if (!is.null(remove_waves)) stopifnot(is.numeric(remove_waves))
  digits <- max(0L, as.integer(digits))
  trim_right <- max(0, min(1, as.numeric(trim_right)))
  return <- match.arg(return)

  # -----------------------------------------------------------------------
  # Normalise input to outcome -> shift -> model (with density ratios)
  normalise_models <- function(obj) {
    if (is.list(obj) && !is.null(obj$models) && is.list(obj$models)) {
      return(obj$models)
    }
    if (is.list(obj) && !is.null(obj$density_ratios)) {
      return(list(`(outcome)` = list(`(shift)` = obj)))
    }
    if (is.numeric(obj)) {
      fake <- list(density_ratios = obj)
      return(list(`(outcome)` = list(`(shift)` = fake)))
    }
    if (is.list(obj) && length(obj) &&
        all(vapply(obj, function(z) is.list(z) && !is.null(z$density_ratios), logical(1)))) {
      return(list(`(outcome)` = obj))
    }
    stop("Unsupported input to `margot_interpret_lmtp_positivity()`. Pass a `margot_lmtp()` result, a single LMTP model, or a numeric vector/matrix of density ratios.")
  }

  models_nested <- normalise_models(x)
  if (!outcome %in% names(models_nested)) {
    stop("Outcome not found in models: ", outcome)
  }
  outcome_models <- models_nested[[outcome]]
  if (!length(outcome_models)) {
    return(if (identical(return, "text")) "" else list())
  }

  clean_shift <- function(name) {
    prefix <- paste0(outcome, "_")
    if (startsWith(name, prefix)) substring(name, nchar(prefix) + 1L) else name
  }
  shift_df <- data.frame(
    shift_full = names(outcome_models),
    stringsAsFactors = FALSE
  )
  shift_df$shift_clean <- vapply(shift_df$shift_full, clean_shift, character(1))

  if (is.null(shifts)) {
    keep_idx <- seq_len(nrow(shift_df))
    shift_df <- shift_df[keep_idx, , drop = FALSE]
    # Default ordering preference when no explicit order supplied
    pref_order <- c("null", "shift_down", "shift_up")
    ord <- c(intersect(pref_order, shift_df$shift_clean),
             setdiff(shift_df$shift_clean, pref_order))
    shift_df <- shift_df[match(ord, shift_df$shift_clean), , drop = FALSE]
  } else {
    # Keep only requested shifts and respect the order provided by `shifts`
    keep_idx <- which(shift_df$shift_full %in% shifts | shift_df$shift_clean %in% shifts)
    if (!length(keep_idx)) {
      stop("Requested shifts not found for outcome ", outcome, ": ", paste(shifts, collapse = ", "))
    }
    shift_df <- shift_df[keep_idx, , drop = FALSE]
    ord_idx <- unlist(lapply(shifts, function(s) {
      which(shift_df$shift_full == s | shift_df$shift_clean == s)[1]
    }))
    ord_idx <- ord_idx[is.finite(ord_idx) & !is.na(ord_idx)]
    if (length(ord_idx)) shift_df <- shift_df[ord_idx, , drop = FALSE]
  }

  # methods text helper ----------------------------------------------------
  methods_text <- function() {
    c(
      "## Understanding Density Ratios in LMTP",
      "",
      "Density ratios act as weights in the estimator to rebalance the observed data to mimic the distribution under the modified policy. A value $r_t > 1$ indicates that the observed treatment $A_t$ is more likely under the modified policy than under the observed mechanism, so the observation is up-weighted at that time point. Conversely, $r_t < 1$ down-weights it.",
      "",
      "Examining the distribution of density ratios allows us to assess practical positivity. Many values near 0 or extremely large suggest violations of the positivity assumption, where certain treatments are improbable under the policy, leading to unstable estimates.",
      "",
      "In a single-time-point average treatment effect (ATE) where the policy sets treatment to 1, the density ratio reduces to the inverse probability weight $1 / \\Pr(A=1 \\mid W)$ for treated units (and 0 for untreated). Where exposures are repeated, the product of ratios across time points gives the overall weight for each observation.",
      "",
      "**Effective Sample Size (ESS)** summarises the effective information in weighted data using the formula: $\\text{ESS} = (\\sum w)^2 / \\sum w^2$. Higher variance in weights reduces ESS. When all weights are equal, $\\text{ESS} = N$ (the actual sample size). $\\text{ESS}/N$ gives the proportion of effective information retained.",
      "",
      "### Censoring vs. Treatment Positivity",
      "",
      "In longitudinal LMTP, zeros ($r_t = 0$) in density ratios primarily reflect **censoring** (dropout) rather than treatment positivity violations. When an individual is censored at time $t$, they have no observed treatment at subsequent waves, yielding $r_t = 0$ in the numerator regardless of the policy. These censoring-induced zeros appear identically across all policies for the same individual.",
      "",
      "In contrast, true **treatment positivity violations** are policy-specific: an observed treatment trajectory may be incompatible with one policy but not another. To distinguish these cases, we focus positivity diagnostics on **uncensored observations** ($r > 0$), where the density ratio reflects actual treatment mechanism compatibility rather than missing data.",
      "",
      "The censoring rate (proportion of $r = 0$) is reported separately per shift to quantify data loss, while ESS and distributional diagnostics are computed only on uncensored observations to assess positivity where treatment was actually observed.",
      "",
      "Note: In this analysis framework, even the `null` policy includes weighting to recover the baseline population via censoring adjustment. As a result, `null` density ratios need not be centred at 1. Departures from 1 under `null` reflect this censoring/stabilisation adjustment rather than a treatment positivity issue.",
      ""
    )
  }

  # ipsi context helper ----------------------------------------------------
  ipsi_context_text <- function() {
    # Determine if any requested/available shifts are IPSI-type
    has_ipsi <- any(grepl("^ipsi(?:_|$)", shift_df$shift_clean, ignore.case = TRUE))
    if (!isTRUE(include_ipsi_context) || !has_ipsi) return(character(0))

    # Extract delta values from shift names like "ipsi_02", "ipsi_5", "ipsi_10", "ipsi_100"
    ipsi_names <- shift_df$shift_clean[grepl("^ipsi(?:_|$)", shift_df$shift_clean, ignore.case = TRUE)]
    parse_delta <- function(nm) {
      # remove leading pattern and any non-numeric characters except '.'
      raw <- sub("^ipsi_?", "", tolower(nm))
      raw <- gsub("[^0-9\\.]", "", raw)
      suppressWarnings(as.numeric(raw))
    }
    deltas <- unique(vapply(ipsi_names, parse_delta, numeric(1)))
    deltas <- deltas[is.finite(deltas)]
    deltas <- sort(deltas)

    # Build LaTeX-ready delta set string if any parsed
    delta_set <- if (length(deltas)) {
      paste0("$\\delta \\in \\{", paste(deltas, collapse = ", "), "\\}$")
    } else {
      NULL
    }

    # Infer treatment label if not provided
    infer_label <- function() {
      # Use keywords in outcome/shift names to pick a sensible domain label
      text <- tolower(paste(c(outcome, shift_df$shift_full, shift_df$shift_clean), collapse = " "))
      if (grepl("attend|attendance|church|service|relig", text)) return("attendance")
      "exposure"
    }
    label_to_use <- if (is.null(treatment_label) || !nzchar(treatment_label)) infer_label() else treatment_label

    # Choose example g values if not provided (domain-aware heuristics)
    if (is.null(ipsi_example_g)) {
      if (identical(label_to_use, "attendance")) {
        example_g_vals <- c(0.02, 0.05, 0.10)
      } else {
        example_g_vals <- c(0.05, 0.10, 0.20)
      }
    } else {
      example_g_vals <- as.numeric(ipsi_example_g)
      example_g_vals <- example_g_vals[is.finite(example_g_vals) & example_g_vals > 0 & example_g_vals < 1]
      if (!length(example_g_vals)) example_g_vals <- c(0.05, 0.10, 0.20)
    }

    # Example translations on the probability scale (illustrative only)
    q_fun <- function(g, d) (d * g) / (1 - g + d * g)
    fmt_num <- function(x) sprintf("%.3f", x)

    example_lines <- character(0)
    if (length(deltas)) {
      # for each example g, show mapping for all deltas present
      for (g in example_g_vals) {
        qs <- vapply(deltas, function(d) fmt_num(q_fun(g, d)), character(1))
        lhs <- sprintf("for $g=%s$:", fmt_num(g))
        rhs <- paste(paste0("ipsi(", deltas, ") $\\to$ ", qs), collapse = ", ")
        example_lines <- c(example_lines, paste(lhs, rhs))
      }
    }

    # Assemble block
    block <- c(
      "## Incremental Propensity Score Interventions (IPSI)",
      "",
      paste0(
        "For binary ", label_to_use, " $A_t$ with observed $g_t(H_t)=\\Pr(A_t=1\\mid H_t)$, ",
        "the IPSI with parameter $\\delta>0$ modifies the assignment mechanism to ",
        "$q_t(H_t) = \\dfrac{\\delta\\, g_t(H_t)}{(1 - g_t(H_t)) + \\delta\\, g_t(H_t)}$, applied at each wave conditional on history $H_t$."
      ),
      "Definitions: $\\delta>0$ is the policy intensity; $g_t(H_t)=\\Pr(A_t=1\\mid H_t)$ is the observed conditional risk at wave $t$; $q_t(H_t)$ is the policy‑modified risk; $H_t$ denotes the measured history at wave $t$; $A_t$ is the binary exposure.",
      if (!is.null(delta_set)) paste0("We considered ", delta_set, ".") else "",
      if (length(example_lines)) "Illustration on the probability scale (not estimates; for intuition only):" else "",
      if (length(example_lines)) paste0("- ", example_lines) else ""
    )
    block[nzchar(block)]
  }

  # deterministic policies context helper ----------------------------------
  deterministic_context_text <- function() {
    if (!isTRUE(include_deterministic_context)) return(character(0))
    # Identify non-ipsi, non-null shifts that look deterministic by naming
    det_idx <- which(!grepl("^ipsi(?:_|$)", shift_df$shift_clean, ignore.case = TRUE) &
                       !grepl("^null$", shift_df$shift_clean, ignore.case = TRUE))
    if (!length(det_idx)) return(character(0))

    # Construct a small list of named policies if available
    det_names <- shift_df$shift_clean[det_idx]
    labeled <- vapply(det_names, map_label, character(1))
    # Provide a one-paragraph description with LaTeX
    lines <- c(
      "## Deterministic Policies",
      "",
      paste0(
        "Deterministic policies modify the current exposure directly via a rule $d_t$, ",
        "yielding $A_t^{\\bar d} := d_t(A_t,\\mathcal H_t)$. These rules may be history-independent ",
        "(e.g., setting a minimum or maximum) or depend on covariate history $\\mathcal H_t$."
      )
    )
    lines <- c(lines, "Definitions: $A_t$ is the exposure at wave $t$; $d_t$ is a policy rule (possibly history‑dependent); $\\mathcal H_t$ denotes the measured history available at wave $t$.")
    if (length(labeled)) {
      # List detected policies in a single line
      lines <- c(lines, paste0("Included deterministic policies: ", paste(unique(labeled), collapse = ", "), "."))
    }
    lines
  }

  # policy-rate context helper ---------------------------------------------
  policy_rate_context_text <- function() {
    if (!isTRUE(include_policy_rates)) return(character(0))
    c(
      "",
      "Policy rates report $\\Pr(A_t=1)$ under each policy by reweighting the observed data. Definitions: $r_{i,t}$ are per‑wave density‑ratio weights; $A_{i,t}$ is the exposure indicator (after thresholding if needed); $\\hat p_t = \\sum_i r_{i,t} A_{i,t} / \\sum_i r_{i,t}$. When the exposure is not binary, we create an indicator $\\mathbb{1}(A_t \\;{op}\\; \\tau)$ before aggregation (defaults: $op$ is $>$ and $\\tau=0$).",
      ""
    )
  }

  # tests explanations helper ----------------------------------------------
  tests_explanations_text <- function() {
    if (!isTRUE(include_test_explanations)) return(character(0))
    c(
      "## Test Explanations",
      "",
      "Near‑zero uniform weights: flags per‑wave when positive ratios are tiny and nearly constant. ESS+/(N+) can look high because it is scale‑invariant; this test highlights rare‑exposure regimes.",
      "Product‑of‑r across waves: the SDR/TMLE estimator reweights by the product of per‑wave ratios along observed histories. We report (i) % zero including censoring (IPCW sets censored rows to zero weight), and (ii) among uncensored rows, the fraction with log10(product) below a threshold (e.g., −1 = 10%). The latter is a practical‑positivity screen.",
      "Censoring adjustment (IPCW): zeros in density‑ratio columns primarily reflect censoring. We up‑weight by inverse censoring probability to target the baseline cohort; high % zeros quantifies censoring burden rather than a treatment‑positivity violation."
    )
  }

  # helpers ----------------------------------------------------------------
  fmt_frac <- function(x) {
    out <- rep("NA", length(x))
    idx <- is.finite(x)
    if (any(idx)) out[idx] <- sprintf(paste0("%.", digits, "f"), x[idx])
    out
  }
  fmt_int <- function(x) {
    formatter <- function(val) format(round(val), big.mark = ",", scientific = FALSE)
    if (exists("pretty_number", mode = "function")) {
      tryCatch(pretty_number(round(x)), error = function(e) formatter(x))
    } else {
      formatter(x)
    }
  }
  # LaTeX sanitiser for common Unicode glyphs
  sanitize_latex <- function(txt) {
    if (!is.character(txt) || !length(txt)) return(txt)
    out <- txt
    # arrows and math symbols
    out <- gsub("→", " $\\\\to$ ", out, fixed = TRUE)
    out <- gsub("±", " $\\\\pm$ ", out, fixed = TRUE)
    out <- gsub("≥", " $\\\\ge$ ", out, fixed = TRUE)
    out <- gsub("≤", " $\\\\le$ ", out, fixed = TRUE)
    out <- gsub("≈", " $\\\\approx$ ", out, fixed = TRUE)
    out <- gsub("×", " $\\\\times$ ", out, fixed = TRUE)
    out
  }
  map_label <- function(lbl) {
    if (exists("transform_label", mode = "function")) {
      out <- tryCatch(
        transform_label(
          label = lbl,
          label_mapping = label_mapping,
          options = list(
            remove_tx_prefix = TRUE,
            remove_z_suffix = TRUE,
            remove_underscores = TRUE,
            use_title_case = TRUE,
            quiet = TRUE
          )
        ),
        error = function(e) lbl
      )
      if (!is.null(out) && !is.na(out)) return(out)
    }
    gsub("_", " ", tools::toTitleCase(lbl))
  }

  # wave label helper -------------------------------------------------------
  wave_label <- function(wv) {
    key <- paste0("wave_", as.integer(wv))
    # try transform_label with provided mapping; fall back to "Wave i"
    if (exists("transform_label", mode = "function")) {
      out <- tryCatch(
        transform_label(
          label = key,
          label_mapping = label_mapping,
          options = list(
            remove_tx_prefix = FALSE,
            remove_z_suffix = FALSE,
            remove_underscores = FALSE,
            use_title_case = FALSE,
            quiet = TRUE
          )
        ),
        error = function(e) NULL
      )
      if (!is.null(out) && is.character(out) && nzchar(out)) return(out)
    }
    paste0("Wave ", as.integer(wv))
  }

  summarise_shift <- function(shift_full, shift_clean) {
    mod <- outcome_models[[shift_full]]
    dr <- mod$density_ratios
    if (is.null(dr)) return(NULL)
    if (inherits(dr, "Matrix")) dr <- as.matrix(dr)
    if (is.vector(dr)) dr <- matrix(dr, ncol = 1L)
    if (!is.matrix(dr)) dr <- as.matrix(dr)
    storage.mode(dr) <- "double"

    # Attempt to obtain exposure-by-wave (must align with density ratios)
    exp_by_wave <- NULL
    if (isTRUE(include_policy_rates) && !is.null(mod$exposure_by_wave)) {
      exp_by_wave <- mod$exposure_by_wave
      if (inherits(exp_by_wave, "Matrix")) exp_by_wave <- as.matrix(exp_by_wave)
      if (is.vector(exp_by_wave)) exp_by_wave <- matrix(exp_by_wave, ncol = 1L)
      if (!is.matrix(exp_by_wave)) exp_by_wave <- try(as.matrix(exp_by_wave), silent = TRUE)
      if (inherits(exp_by_wave, "try-error")) exp_by_wave <- NULL
    }

    total_cols <- ncol(dr)
    if (total_cols == 0L) return(NULL)

    col_idx <- seq_len(total_cols)
    if (!is.null(waves)) col_idx <- intersect(col_idx, as.integer(waves))
    if (!is.null(remove_waves)) col_idx <- setdiff(col_idx, as.integer(remove_waves))
    if (!length(col_idx)) return(NULL)

    sub_dr <- dr[, col_idx, drop = FALSE]
    # Ensure matrix structure even when a single column is selected
    if (!is.matrix(sub_dr)) sub_dr <- matrix(sub_dr, ncol = 1L)

    sub_mask <- is.finite(sub_dr) & (sub_dr > 0)
    sub_dr_masked <- sub_dr
    sub_dr_masked[!sub_mask] <- NA_real_

    sub_dr_trim <- sub_dr_masked
    if (trim_right > 0 && trim_right < 1) {
      for (j in seq_len(ncol(sub_dr_trim))) {
        col_vals <- sub_dr_trim[, j]
        finite_vals <- col_vals[is.finite(col_vals)]
        if (length(finite_vals)) {
          q <- stats::quantile(finite_vals, probs = trim_right, names = FALSE)
          sub_dr_trim[, j] <- ifelse(is.finite(col_vals), pmin(col_vals, q), NA_real_)
        }
      }
    }

    ess_fun <- function(w) {
      w <- w[is.finite(w) & (w > 0)]
      if (!length(w)) return(NA_real_)
      s1 <- sum(w)
      s2 <- sum(w^2)
      if (s2 <= 0) return(NA_real_)
      (s1^2) / s2
    }

    cumulate <- function(mat) {
      res <- matrix(NA_real_, nrow(mat), ncol(mat))
      if (!ncol(mat)) return(res)
      res[, 1] <- mat[, 1]
      if (ncol(mat) >= 2) {
        for (j in 2:ncol(mat)) {
          prev <- res[, j - 1]
          curr <- mat[, j]
          res[, j] <- ifelse(is.finite(prev) & is.finite(curr), prev * curr, NA_real_)
        }
      }
      res
    }

    cum_raw <- cumulate(sub_dr_masked)
    cum_trim <- cumulate(sub_dr_trim)

    # align exposure-by-wave if available; otherwise keep NULL
    sub_exp <- NULL
    if (!is.null(exp_by_wave)) {
      # try to align on columns; truncate or subset if needed
      if (ncol(exp_by_wave) >= length(col_idx)) {
        sub_exp <- exp_by_wave[, col_idx, drop = FALSE]
      }
      # basic sanity: same nrow as dr
      if (!is.null(sub_exp)) {
        ok_dim <- nrow(sub_exp) == nrow(sub_dr)
        if (!ok_dim) {
          sub_exp <- NULL
        } else {
          # If not already in [0,1], map to binary using threshold
          rng <- range(sub_exp[is.finite(sub_exp)], na.rm = TRUE)
          if (!(is.finite(rng[1]) && is.finite(rng[2]) && rng[1] >= 0 && rng[2] <= 1)) {
            comp <- if (isTRUE(policy_rate_strict)) function(a,b) a > b else function(a,b) a >= b
            sub_exp <- ifelse(comp(sub_exp, policy_rate_threshold), 1, 0)
          }
        }
      }
    }

    # compute metrics per wave using uncensored observations (r > 0)
    wave_metrics <- lapply(seq_along(col_idx), function(i) {
      w <- sub_dr_masked[, i]
      mask_col <- sub_mask[, i]
      n_total <- sum(is.finite(sub_dr[, i]))
      n_obs <- sum(mask_col, na.rm = TRUE)
      prop_censored <- if (n_total > 0) (n_total - n_obs) / n_total else NA_real_

      w_pos <- w[is.finite(w)]
      n_pos <- length(w_pos)

      ess_pos <- ess_fun(w_pos)
      ess_pos_frac <- if (n_pos > 0 && is.finite(ess_pos)) ess_pos / n_pos else NA_real_
      ess_pos_frac_pt <- if (n_total > 0 && is.finite(ess_pos)) ess_pos / n_total else NA_real_

      ess_cum_raw <- ess_fun(cum_raw[, i])
      ess_cum_trim <- ess_fun(cum_trim[, i])

      # policy-implied exposure probability by wave (uncensored)
      p_hat <- NA_real_
      if (!is.null(sub_exp)) {
        e <- sub_exp[, i]
        mask_e <- is.finite(sub_dr[, i]) & is.finite(e) & mask_col
        if (any(mask_e)) {
          sw <- sum(sub_dr[, i][mask_e])
          if (sw > 0) p_hat <- sum(sub_dr[, i][mask_e] * e[mask_e]) / sw
        }
      }

      list(
        wave = col_idx[i],
        n_all = n_total,
        n_pos = n_pos,
        prop_censored = prop_censored,
        ess_pos = ess_pos,
        ess_pos_frac = ess_pos_frac,
        ess_pos_frac_pt = ess_pos_frac_pt,
        ess_cum_raw = ess_cum_raw,
        ess_cum_trim = ess_cum_trim,
        policy_rate = p_hat
      )
    })

    wave_df <- do.call(rbind, lapply(wave_metrics, function(m) {
      data.frame(
        shift_full = shift_full,
        shift_clean = shift_clean,
        wave = m$wave,
        n_all = m$n_all,
        n_pos = m$n_pos,
        prop_censored = m$prop_censored,
        ess_pos = m$ess_pos,
        ess_pos_frac = m$ess_pos_frac,
        ess_pos_frac_pt = m$ess_pos_frac_pt,
        ess_cum_raw = m$ess_cum_raw,
        ess_cum_trim = m$ess_cum_trim,
        policy_rate = m$policy_rate,
        stringsAsFactors = FALSE
      )
    }))

    # overall metrics across waves (uncensored)
    all_w <- as.vector(sub_dr)
    overall_n_all <- sum(is.finite(all_w))
    overall_n_obs <- sum(sub_mask, na.rm = TRUE)
    overall_prop_censored <- if (overall_n_all > 0) (overall_n_all - overall_n_obs) / overall_n_all else NA_real_

    all_w_pos <- as.vector(sub_dr_masked)
    all_w_pos <- all_w_pos[is.finite(all_w_pos)]
    overall_n_pos <- length(all_w_pos)

    overall_ess_pos <- ess_fun(all_w_pos)

    overall_ess_pos_frac <- if (overall_n_pos > 0 && is.finite(overall_ess_pos)) overall_ess_pos / overall_n_pos else NA_real_
    overall_ess_pos_frac_pt <- if (overall_n_all > 0 && is.finite(overall_ess_pos)) overall_ess_pos / overall_n_all else NA_real_

    overall_ess_cum_raw <- ess_fun(cum_raw[, ncol(cum_raw), drop = TRUE])
    overall_ess_cum_trim <- ess_fun(cum_trim[, ncol(cum_trim), drop = TRUE])

    # overall policy-implied exposure probability across selected waves (uncensored)
    overall_policy_rate <- NA_real_
    if (!is.null(sub_exp)) {
      e_all <- as.vector(sub_exp)
      mask_all <- is.finite(all_w) & is.finite(e_all) & (all_w > 0)
      if (any(mask_all)) {
        sw <- sum(all_w[mask_all])
        if (sw > 0) overall_policy_rate <- sum(all_w[mask_all] * e_all[mask_all]) / sw
      }
    }

    list(
      waves = wave_df,
      overall = data.frame(
        shift_full = shift_full,
        shift_clean = shift_clean,
        n_all = overall_n_all,
        n_pos = overall_n_pos,
        prop_censored = overall_prop_censored,
        ess_pos = overall_ess_pos,
        ess_pos_frac = overall_ess_pos_frac,
        ess_pos_frac_pt = overall_ess_pos_frac_pt,
        ess_cum_raw = overall_ess_cum_raw,
        ess_cum_trim = overall_ess_cum_trim,
        policy_rate = overall_policy_rate,
        stringsAsFactors = FALSE
      ),
      label = map_label(shift_clean)
    )
  }

  shift_results <- lapply(seq_len(nrow(shift_df)), function(i) {
    summarise_shift(shift_df$shift_full[i], shift_df$shift_clean[i])
  })
  names(shift_results) <- shift_df$shift_clean
  shift_results <- Filter(Negate(is.null), shift_results)
  if (!length(shift_results)) {
    return(if (identical(return, "text")) "" else list())
  }

  # additional overlap/positivity tests (CLI + text) and IPSI recommendation
  tests_section <- character(0)
  ipsi_recommend_section <- character(0)
  if (isTRUE(include_tests) || isTRUE(include_ipsi_recommend)) {
    thr <- list(
      near_zero_median = 1e-3,
      near_zero_cv = 0.05,
      prod_log10 = -8,
      prod_frac_warn = 0.20
    )
    if (is.list(test_thresholds) && length(test_thresholds)) {
      for (nm in intersect(names(test_thresholds), names(thr))) thr[[nm]] <- test_thresholds[[nm]]
    }
    safe_cv <- function(v) {
      v <- v[is.finite(v)]
      if (!length(v)) return(NA_real_)
      mu <- mean(v)
      if (!is.finite(mu) || mu == 0) return(NA_real_)
      stats::sd(v) / mu
    }
    # helper to compute metrics per shift (near-zero flags and product-of-r)
    compute_metrics <- function(nm) {
      wdf <- shift_results[[nm]]$waves
      mod <- outcome_models[[wdf$shift_full[1]]]
      dr <- mod$density_ratios
      if (inherits(dr, "Matrix")) dr <- as.matrix(dr)
      if (!is.matrix(dr)) dr <- as.matrix(dr)
      cols <- if (is.null(waves)) seq_len(ncol(dr)) else intersect(seq_len(ncol(dr)), as.integer(waves))
      near_zero <- list()
      for (j in cols) {
        w <- dr[, j]
        w_pos <- w[is.finite(w) & (w > 0)]
        if (!length(w_pos)) next
        med <- stats::median(w_pos)
        cv  <- safe_cv(w_pos)
        if (!is.na(med) && med < thr$near_zero_median && !is.na(cv) && cv < thr$near_zero_cv) near_zero[[length(near_zero) + 1L]] <- list(wave = j, median = med, cv = cv)
      }
      if (!length(cols)) return(list(near_zero = near_zero, prop_zero_prod = NA_real_, frac_below = NA_real_))
      sub <- dr[, cols, drop = FALSE]
      prod_all <- apply(sub, 1L, function(row) {
        row <- row[is.finite(row)]
        if (!length(row)) return(NA_real_)
        if (any(row == 0)) return(0)
        exp(sum(log(row)))
      })
      prod_pos <- apply(sub, 1L, function(row) {
        row <- row[is.finite(row) & row > 0]
        if (!length(row)) return(NA_real_)
        exp(sum(log(row)))
      })
      prop_zero_prod <- mean(prod_all == 0, na.rm = TRUE)
      log10_pos <- suppressWarnings(log10(prod_pos))
      frac_below <- mean(is.finite(log10_pos) & (log10_pos < thr$prod_log10), na.rm = TRUE)
      list(near_zero = near_zero, prop_zero_prod = prop_zero_prod, frac_below = frac_below)
    }
    metrics <- lapply(names(shift_results), compute_metrics)
    names(metrics) <- names(shift_results)

    if (isTRUE(include_tests)) {
      # emit messages and assemble test section
      for (nm in names(shift_results)) {
        f <- metrics[[nm]]$near_zero
        if (length(f)) {
          for (j in seq_along(f)) {
            wave <- f[[j]]$wave; med <- f[[j]]$median; cv <- f[[j]]$cv
            if (requireNamespace("cli", quietly = TRUE)) {
              cli::cli_alert_info("Near-zero uniform weights (outcome={outcome}, shift={nm}, wave={wave}): median={signif(med, 3)}, CV={signif(cv, 3)}.")
              cli::cli_alert_info("ESS+/(N+) ≈ 1 can hide collapse when multiplying across waves; see product-of-r check.")
            }
            tests_section <- c(tests_section,
              paste0("- Near-zero uniform weights — ", map_label(nm), ", ", wave_label(wave), ": median=", signif(med, 3), ", CV=", signif(cv, 3)))
          }
        }
        prop_zero_prod <- metrics[[nm]]$prop_zero_prod
        frac_below <- metrics[[nm]]$frac_below
        if (requireNamespace("cli", quietly = TRUE)) {
          if (is.finite(frac_below) && frac_below > thr$prod_frac_warn) {
            cli::cli_alert_warning("Product-of-r collapse (outcome={outcome}, shift={nm}): {round(100*frac_below,1)}% of uncensored rows have log10(prod r) < {thr$prod_log10}; {round(100*prop_zero_prod,1)}% collapse to zero including censoring.")
          } else {
            cli::cli_alert_info("Product-of-r summary (outcome={outcome}, shift={nm}): {round(100*prop_zero_prod,1)}% zero (with censoring); {round(100*frac_below,1)}% uncensored < 10^{thr$prod_log10}.")
          }
        }
        verdict <- if (is.finite(frac_below) && (frac_below <= thr$prod_frac_warn)) "Pass" else "Fail"
        tests_section <- c(tests_section,
          paste0("- Product-of-r — ", map_label(nm), ": ", round(100*prop_zero_prod,1), "% zero (with censoring); ",
                 round(100*frac_below,1), "% uncensored < 10^{", thr$prod_log10, "} ",
                 "(verdict: ", verdict, ")"))
      }
    }

    # monotone support: shift_weekly ≤ shift_zero (when both present)
    prs <- names(shift_results)
    if (all(c("shift_zero","shift_weekly") %in% prs)) {
      z <- shift_results[["shift_zero"]]$waves
      w <- shift_results[["shift_weekly"]]$waves
      mm <- merge(z[, c("wave","n_pos")], w[, c("wave","n_pos")], by = "wave", suffixes = c("_zero","_weekly"))
      bad <- mm$n_pos_weekly > mm$n_pos_zero
      if (any(bad, na.rm = TRUE)) {
        if (requireNamespace("cli", quietly = TRUE)) {
          cli::cli_alert_warning("Monotone support: weekly exceeds zero at waves {paste(mm$wave[bad], collapse=", ")}. Check shift definitions.")
        }
        tests_section <- c(tests_section, paste0("- Monotone support: FAILED weekly ≤ zero at waves ", paste(mm$wave[bad], collapse=", "), "."))
      } else if (nrow(mm)) {
        if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_info("Monotone support OK: weekly ≤ zero at all overlapping waves.")
        tests_section <- c(tests_section, "- Monotone support: OK (weekly ≤ zero for all waves)")
      }
    }
    # IPSI recommendation: choose largest δ passing guardrails
    if (isTRUE(include_ipsi_recommend)) {
      is_ipsi <- function(nm) grepl("^ipsi(?:_|$)", nm, ignore.case = TRUE)
      ipsi_names <- names(shift_results)[vapply(names(shift_results), is_ipsi, logical(1))]
      if (length(ipsi_names)) {
        parse_delta <- function(nm) {
          raw <- sub("^ipsi_?", "", tolower(nm))
          raw <- gsub("[^0-9\\.]", "", raw)
          suppressWarnings(as.numeric(raw))
        }
        deltas <- vapply(ipsi_names, parse_delta, numeric(1))
        frac_below_vec <- vapply(ipsi_names, function(nm) metrics[[nm]]$frac_below, numeric(1))
        prop_zero_vec  <- vapply(ipsi_names, function(nm) metrics[[nm]]$prop_zero_prod, numeric(1))
        near_zero_ct   <- vapply(ipsi_names, function(nm) length(metrics[[nm]]$near_zero), integer(1))
        passes <- is.finite(frac_below_vec) & (frac_below_vec <= thr$prod_frac_warn)
        rec_idx <- NA_integer_
        if (any(passes, na.rm = TRUE)) {
          idx_pass <- which(passes)
          rec_idx <- idx_pass[which.max(deltas[idx_pass])]
        } else if (any(is.finite(frac_below_vec))) {
          min_val <- min(frac_below_vec[is.finite(frac_below_vec)], na.rm = TRUE)
          cand <- which(is.finite(frac_below_vec) & frac_below_vec == min_val)
          rec_idx <- cand[which.min(deltas[cand])]  # conservative
        }
        if (is.finite(rec_idx) && !is.na(rec_idx)) {
          rec_name <- ipsi_names[rec_idx]
          rec_delta <- deltas[rec_idx]
          rec_pass  <- passes[rec_idx]
          rec_msg <- if (rec_pass) {
            paste0("Recommended δ = ", rec_delta, " (", map_label(rec_name), ") — passes guardrails (≤ ", 100*thr$prod_frac_warn, "% with product < 10^{", thr$prod_log10, "}).")
          } else {
            paste0("No candidate passes guardrails; choose δ = ", rec_delta, " (", map_label(rec_name), ") with least product-of-r shortfall.")
          }
          if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_info(rec_msg)
          ipsi_lines <- character(0)
          for (k in order(deltas)) {
            ipsi_lines <- c(ipsi_lines,
              paste0("- ", map_label(ipsi_names[k]), " (δ=", deltas[k], "): ",
                     round(100*prop_zero_vec[k],1), "% zero; ", round(100*frac_below_vec[k],1),
                     "% uncensored < 10^{", thr$prod_log10, "}; near-zero flags = ", near_zero_ct[k]))
          }
          ipsi_recommend_section <- c("## IPSI Recommendation", rec_msg, "", ipsi_lines)
        }
      }
    }
  }

  # diagnostics helper -----------------------------------------------------
  diagnostics_text <- function() {
    if (!isTRUE(include_diagnostics)) return(character(0))

    # call margot_lmtp_positivity() to get detailed diagnostics
    # note: explicitly rebuild dataframe structure to ensure we have updated columns
    pos_result <- tryCatch({
      result <- margot_lmtp_positivity(x, verbose = FALSE)
      # verify required columns exist
      if (!is.null(result$by_wave)) {
        required <- c("min_pos", "max_pos", "mean_pos", "sd_pos", "cv_pos")
        if (!all(required %in% names(result$by_wave))) {
          warning("margot_lmtp_positivity() missing *_pos columns. Detailed diagnostics unavailable.")
          return(NULL)
        }
      }
      result
    }, error = function(e) {
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_alert_warning("Could not compute detailed diagnostics: {e$message}")
      }
      return(NULL)
    })

    if (is.null(pos_result)) return(character(0))

    # filter to selected outcome and shifts
    by_wave <- pos_result$by_wave
    by_wave <- by_wave[by_wave$outcome == outcome, , drop = FALSE]
    if (!is.null(shifts)) {
      keep <- by_wave$shift %in% shifts |
              sapply(by_wave$shift, function(s) {
                clean <- if (startsWith(s, paste0(outcome, "_"))) {
                  substring(s, nchar(outcome) + 2L)
                } else s
                clean %in% shifts
              })
      by_wave <- by_wave[keep, , drop = FALSE]
    }
    if (!is.null(waves)) {
      by_wave <- by_wave[by_wave$wave %in% waves, , drop = FALSE]
    }
    if (!is.null(remove_waves)) {
      by_wave <- by_wave[!(by_wave$wave %in% remove_waves), , drop = FALSE]
    }

    if (!nrow(by_wave)) return(character(0))

    # build detailed text per shift with wave-by-wave diagnostics (uncensored)
    diag_lines <- character(0)
    for (shift_name in names(shift_results)) {
      shift_full <- shift_results[[shift_name]]$waves$shift_full[1]
      shift_data <- by_wave[by_wave$shift == shift_full, , drop = FALSE]
      if (!nrow(shift_data)) next

      shift_label <- shift_results[[shift_name]]$label
      diag_lines <- c(diag_lines, paste0("### ", shift_label, " — Wave-by-wave Diagnostics (positivity; uncensored rows)"))
      diag_lines <- c(diag_lines, "  note: positivity diagnostics only; point estimates target the baseline cohort via censoring adjustment.")

      # iterate over waves in order
      waves_ord <- order(suppressWarnings(as.integer(shift_data$wave)))
      shift_data <- shift_data[waves_ord, , drop = FALSE]
      for (i in seq_len(nrow(shift_data))) {
        row <- shift_data[i, ]
        wv <- row$wave
        wv_label <- wave_label(wv)
        zeros_pct <- as.numeric(row$prop_zero) * 100
        # per-wave uncensored metrics
        range_min_pos <- as.numeric(row$min_pos)
        range_max_pos <- as.numeric(row$max_pos)
        mean_val_pos <- as.numeric(row$mean_pos)
        sd_val_pos <- as.numeric(row$sd_pos)
        cv_val_pos <- as.numeric(row$cv_pos)

        # tails (uncensored)
        tail_cols <- grep("^p_gt_.*_pos$", names(row), value = TRUE)
        tail_vals <- if (length(tail_cols)) sapply(tail_cols, function(col) as.numeric(row[[col]]) * 100) else NULL
        # quantiles (uncensored)
        quant_cols <- grep("^q.*_pos$", names(row), value = TRUE)
        quants <- if (length(quant_cols)) sapply(quant_cols, function(col) as.numeric(row[[col]])) else NULL

        # Clarify censoring semantics: to next wave except final wave (end-of-study)
        cens_label <- if (i < nrow(shift_data)) "censoring to next wave = " else "censoring end of study = "
        line1 <- paste0("- ", wv_label, ": ", cens_label, fmt_frac(zeros_pct), "%; ",
                        "range [", fmt_frac(range_min_pos), ", ", fmt_frac(range_max_pos), "];")
        line2 <- paste0("  mean $\\pm$ SD = ", fmt_frac(mean_val_pos), " $\\pm$ ", fmt_frac(sd_val_pos),
                        " (CV = ", fmt_frac(cv_val_pos), ")")
        diag_lines <- c(diag_lines, line1, line2)

        if (!is.null(tail_vals) && length(tail_vals)) {
          names_clean <- gsub("^p_gt_", "", gsub("_pos$", "", names(tail_vals)))
          tail_text <- paste(paste0("P(r > ", names_clean, ")"), "=", paste0(fmt_frac(tail_vals), "%"), collapse = ", ")
          diag_lines <- c(diag_lines, paste0("  tails: ", tail_text))
        }

        if (!is.null(quants) && length(quants)) {
          q_labels <- c("q0001_pos" = "p0.1%", "q001_pos" = "p1%", "q005_pos" = "p5%", "q05_pos" = "p50%",
                        "q095_pos" = "p95%", "q0999_pos" = "p99.9%")
          available_q <- intersect(names(quants), names(q_labels))
          if (length(available_q)) {
            q_text <- paste(q_labels[available_q], "=", fmt_frac(quants[available_q]), collapse = ", ")
            diag_lines <- c(diag_lines, paste0("  quantiles: ", q_text))
          }
        }
      }

      diag_lines <- c(diag_lines, "")
    }

    if (length(diag_lines)) {
      c("", "## Detailed Diagnostics by Shift", "", diag_lines)
    } else {
      character(0)
    }
  }

  # Reference counts from the first shift for baseline N / person-time
  ref_waves <- shift_results[[1]]$waves
  baseline_n <- if (nrow(ref_waves)) max(ref_waves$n_all, na.rm = TRUE) else NA_real_
  person_time <- if (nrow(ref_waves)) sum(ref_waves$n_all, na.rm = TRUE) else NA_real_

  outcome_label <- map_label(outcome)

  header_bits <- c(
    paste0("LMTP positivity diagnostics for ", outcome_label, " (positivity on uncensored rows)."),
    "Estimation reweights to the baseline cohort via censoring adjustment.",
    if (is.finite(baseline_n)) paste0("Baseline N $\\approx$ ", fmt_int(baseline_n), ".") else "",
    if (is.finite(person_time)) paste0("Person-time rows per shift (selected waves) = ", fmt_int(person_time), ".") else ""
  )
  header <- paste(header_bits[nzchar(header_bits)], collapse = " ")

  # overview lines removed - averaging across interventions is incoherent
  overview_lines <- character(0)

  # per-shift precision lines (ESS = precision, not positivity) ------------
  lines <- vapply(names(shift_results), function(name) {
    res <- shift_results[[name]]
    wave_df <- res$waves
    overall_df <- res$overall
    if (!nrow(wave_df)) return("")

    # uncensored ESS by wave (precision)
    wave_names <- vapply(wave_df$wave, wave_label, character(1))
    wave_bits <- paste0(
      wave_names, ": ESS = ", fmt_int(wave_df$ess_pos),
      " (ESS+/(N+) = ", fmt_frac(wave_df$ess_pos_frac),
      ", ESS+/(N_pt) = ", fmt_frac(wave_df$ess_pos_frac_pt), "); ",
      "cum ESS = ", fmt_int(wave_df$ess_cum_raw),
      ifelse(is.finite(wave_df$ess_cum_trim),
             paste0(" (trim ", fmt_int(wave_df$ess_cum_trim), ")"),
             "")
    )
    wave_clause <- paste(wave_bits, collapse = ", ")

    # overall uncensored ESS
    overall_clause <- ""
    if (is.finite(overall_df$ess_pos)) {
      overall_clause <- paste0(
        "; overall ESS = ", fmt_int(overall_df$ess_pos),
        " (ESS+/(N+) = ", fmt_frac(overall_df$ess_pos_frac), ")"
      )
      if ("ess_pos_frac_pt" %in% names(overall_df) && is.finite(overall_df$ess_pos_frac_pt)) {
        overall_clause <- paste0(overall_clause, "; ESS+/(N_pt) = ", fmt_frac(overall_df$ess_pos_frac_pt))
      }
      if ("ess_cum_raw" %in% names(overall_df) && is.finite(overall_df$ess_cum_raw)) {
        overall_clause <- paste0(overall_clause, "; cumulative ESS = ", fmt_int(overall_df$ess_cum_raw))
        if ("ess_cum_trim" %in% names(overall_df) && is.finite(overall_df$ess_cum_trim)) {
          overall_clause <- paste0(overall_clause, " (trim ", fmt_int(overall_df$ess_cum_trim), ")")
        }
      }
    }
    # policy rates (optional)
    policy_clause <- ""
    if (isTRUE(include_policy_rates) && "policy_rate" %in% names(wave_df)) {
      if (any(is.finite(wave_df$policy_rate))) {
        pr_bits <- paste0(vapply(wave_df$wave, wave_label, character(1)), ": ", fmt_frac(wave_df$policy_rate))
        pr_clause <- paste(pr_bits, collapse = ", ")
        pr_overall <- if ("policy_rate" %in% names(overall_df) && is.finite(overall_df$policy_rate)) paste0("; overall = ", fmt_frac(overall_df$policy_rate)) else ""
        policy_clause <- paste0("; policy $\\Pr(A_t=1)$ by wave — ", pr_clause, pr_overall)
      }
    }

    paste0("- ", res$label, ": ESS by wave — ", wave_clause, overall_clause, policy_clause)
  }, character(1))
  lines <- lines[nzchar(lines)]

  # assemble final output with optional methods and diagnostics
  methods_section <- if (isTRUE(include_methods)) methods_text() else character(0)
  ipsi_section <- ipsi_context_text()
  det_section <- deterministic_context_text()
  policy_section <- policy_rate_context_text()
  diagnostics_section <- diagnostics_text()  # returns empty vector if include_diagnostics = FALSE

  # include IPSI recommendation, tests intro/explanations, and tests (if present)
  ipsi_block <- if (length(ipsi_recommend_section)) c("", ipsi_recommend_section) else character(0)
  tests_intro <- if (length(tests_section)) c(
    "",
    "## Positivity/Overlap Tests",
    "",
    "Censoring is handled via inverse probability weighting (IPCW) to target the baseline cohort. The 'zero %' below quantifies the censoring burden across person‑time; the 'uncensored < 10^k' fraction screens treatment‑positivity among observed rows."
  ) else character(0)
  tests_expl_block <- tests_explanations_text()
  tests_block <- if (length(tests_section)) c(tests_intro, tests_expl_block, tests_section) else character(0)

  precision_block <- if (length(lines)) c(
    "",
    "## Precision (weights)",
    "",
    "ESS/N summaries describe weight variability (precision). Lower values mean noisier estimates; they are not positivity tests.",
    lines
  ) else character(0)

  text_lines <- c(methods_section, ipsi_section, det_section, policy_section, header, ipsi_block, tests_block, overview_lines, precision_block, diagnostics_section)
  text <- paste(text_lines, collapse = "\n")
  text <- sanitize_latex(text)

  if (identical(return, "text")) {
    text
  } else {
    list(
      text = text,
      methods = if (isTRUE(include_methods)) paste(methods_section, collapse = "\n") else NULL,
      header = header,
      overview = overview_lines,
      lines = lines,
      diagnostics = if (isTRUE(include_diagnostics)) paste(diagnostics_section, collapse = "\n") else NULL,
      outcome_label = outcome_label,
      shifts = shift_results,
      baseline_n = baseline_n,
      person_time = person_time
    )
  }
}

#' @rdname margot_interpret_lmtp_positivity
#' @export
margot_interpret_lmtp_overlap <- function(...) {
  msg <- "`margot_interpret_lmtp_overlap()` is soft-deprecated; use `margot_interpret_lmtp_positivity()` instead."
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_warn(msg)
  } else {
    warning(msg, call. = FALSE)
  }
  margot_interpret_lmtp_positivity(...)
}
