#' Conditional-moment concentration for a discrete intervention
#'
#' Assess the concentration of uncapped exposure-only ratios for one arm and
#' assigned prefix. Observation probabilities define a separate averaging
#' measure. Supply the study's threshold explicitly.
#'
#' @param natural_mass,policy_mass Numeric probability matrices. Rows have
#'   names equal to row_id; matching column names identify discrete values
#'   or regions with a common ratio within each region. Each reached row sums
#'   to one, within a tolerance of 1e-12.
#' @param preceding_weight Named numeric vector of products of exposure ratios
#'   before this assignment. Use ones for the first assignment.
#' @param reached Named logical vector marking the current observation risk set.
#' @param row_id Unique nonempty character identifiers in input order.
#' @param minimum Study-selected concentration threshold in (0, 1]. Equality
#'   passes. This is a decision convention, not a validity guarantee.
#' @param observation_weight Optional positive, named inverse-observation
#'   weights. They are normalised over all reached records, including records
#'   with zero preceding exposure weight. NULL uses equal averaging weights.
#' @return A list with fraction, equivalent (fraction times reached count),
#'   n, and state (pass, fail, boundary_fail, or unassessable).
#'   Assessed inputs also give boundary_records and sole_failures.
#'   A required positive policy mass with zero natural mass on a positive-weight
#'   row gives boundary_fail and no fraction. Invalid or numerically
#'   unrepresentable inputs give unassessable.
#' @details With normalised observation weights b, preceding exposure ratios W
#'   and m2 = sum(q^2/g), the fraction is
#'   (sum(b * W))^2 / sum(b * W^2 * m2). The observation weights enter once.
#'   Zero-W records remain in the averaging population; their current
#'   exposure divisions are skipped. Inputs are never trimmed. The fraction
#'   assesses the supplied fitted laws; it cannot establish positivity,
#'   identification or coverage.
#' @examples
#' ids <- c("a", "b")
#' g <- matrix(c(.2, .8, .2, .8), 2, byrow = TRUE,
#'   dimnames = list(ids, c("zero", "positive")))
#' q <- g
#' q[, ] <- rep(c(1, 0), each = 2)
#' margot_lmtp_support_concentration(g, q, setNames(c(1, 1), ids),
#'   setNames(c(TRUE, TRUE), ids), ids, minimum = .05)
#' @export
margot_lmtp_support_concentration <- function(natural_mass, policy_mass,
                                            preceding_weight, reached, row_id,
                                            minimum, observation_weight = NULL) {
  if (!is.numeric(minimum) || length(minimum) != 1L || !is.finite(minimum) ||
      minimum <= 0 || minimum > 1) {
    stop("minimum must be a single number in (0, 1].", call. = FALSE)
  }
  n <- if (is.logical(reached) && !anyNA(reached)) sum(reached) else NA_integer_
  unassessable <- list(fraction = NA_real_, equivalent = NA_real_, n = n,
                      state = "unassessable")

  # ordered identities cover every input, including rows outside the risk set.
  valid_ids <- is.character(row_id) && !anyNA(row_id) &&
    all(nzchar(row_id)) && !anyDuplicated(row_id)
  if (!valid_ids || !is.matrix(natural_mass) || !is.numeric(natural_mass) ||
      !is.matrix(policy_mass) || !is.numeric(policy_mass) ||
      !identical(dim(natural_mass), dim(policy_mass)) ||
      nrow(natural_mass) != length(row_id) || ncol(natural_mass) == 0L ||
      !identical(rownames(natural_mass), row_id) ||
      !identical(rownames(policy_mass), row_id) ||
      is.null(colnames(natural_mass)) || anyNA(colnames(natural_mass)) ||
      any(!nzchar(colnames(natural_mass))) || anyDuplicated(colnames(natural_mass)) ||
      !identical(colnames(natural_mass), colnames(policy_mass)) ||
      !is.numeric(preceding_weight) || length(preceding_weight) != length(row_id) ||
      !identical(names(preceding_weight), row_id) ||
      !is.logical(reached) || length(reached) != length(row_id) || anyNA(reached) ||
      !identical(names(reached), row_id) || is.na(n) || n == 0L) {
    return(unassessable)
  }

  # optional IPC weights define the averaging measure, including zero-exposure-weight rows.
  if (!is.null(observation_weight)) {
    if (!is.numeric(observation_weight) || length(observation_weight) != length(row_id) ||
        !identical(names(observation_weight), row_id) ||
        any(!is.finite(observation_weight[reached])) || any(observation_weight[reached] <= 0)) {
      return(unassessable)
    }
    averaging <- observation_weight[reached] / max(observation_weight[reached])
    averaging <- averaging / sum(averaging)
    if (any(!is.finite(averaging)) || any(averaging <= 0)) return(unassessable)
  }

  natural_mass <- natural_mass[reached, , drop = FALSE]
  policy_mass <- policy_mass[reached, , drop = FALSE]
  preceding_weight <- preceding_weight[reached]
  # probability-law validation has a separate numerical tolerance from the decision threshold.
  valid_law <- function(mass) {
    all(is.finite(mass)) && all(mass >= 0 & mass <= 1) &&
      all(abs(rowSums(mass) - 1) <= 1e-12)
  }
  if (!valid_law(natural_mass) || !valid_law(policy_mass) ||
      any(!is.finite(preceding_weight)) || any(preceding_weight < 0) ||
      !any(preceding_weight > 0)) {
    return(unassessable)
  }

  # common rescaling leaves T^2/V unchanged and avoids squaring large weights.
  positive <- preceding_weight > 0
  weight <- preceding_weight[positive] / max(preceding_weight)
  if (any(weight == 0) || any(weight^2 == 0)) return(unassessable)
  natural_mass <- natural_mass[positive, , drop = FALSE]
  policy_mass <- policy_mass[positive, , drop = FALSE]
  second_moment <- numeric(length(weight))
  boundary <- logical(length(weight))
  for (i in seq_along(weight)) {
    # zero preceding weights and zero policy masses never enter a division.
    required <- policy_mass[i, ] > 0
    g <- natural_mass[i, required]
    q <- policy_mass[i, required]
    # a required value with zero fitted natural mass is a distinct support failure.
    if (any(g == 0)) {
      boundary[i] <- TRUE
      next
    }
    term <- q * (q / g)
    if (any(!is.finite(term)) || any(term == 0)) return(unassessable)
    second_moment[i] <- sum(term)
  }
  if (any(boundary)) {
    return(list(fraction = NA_real_, equivalent = NA_real_, n = n,
      state = "boundary_fail", reason = "required mass has zero fitted support",
      boundary_records = sum(boundary), sole_failures = NA_integer_))
  }
  contribution <- weight^2 * second_moment
  if (is.null(observation_weight)) {
    total_weight <- sum(weight)
    total_moment <- sum(contribution)
  } else {
    weighted_value <- averaging[positive] * weight
    weighted_moment <- averaging[positive] * contribution
    if (any(weighted_value == 0) || any(weighted_moment == 0)) return(unassessable)
    total_weight <- sum(weighted_value)
    total_moment <- sum(weighted_moment)
  }
  if (any(!is.finite(contribution)) || any(contribution == 0) ||
      !is.finite(total_weight) || total_weight <= 0 ||
      !is.finite(total_moment) || total_moment <= 0) {
    return(unassessable)
  }
  if (is.null(observation_weight)) {
    equivalent <- total_weight^2 / total_moment
    fraction <- total_weight^2 / (n * total_moment)
  } else {
    fraction <- total_weight^2 / total_moment
    equivalent <- n * fraction
  }
  if (!is.finite(equivalent) || equivalent <= 0 ||
      !is.finite(fraction) || fraction <= 0) return(unassessable)
  # records whose contribution alone would put the fraction below the threshold.
  sole <- if (is.null(observation_weight)) contribution > total_weight^2 / (minimum * n) else
    averaging[positive] * contribution > total_weight^2 / minimum
  list(fraction = fraction, equivalent = equivalent, n = n,
       state = if (fraction >= minimum) "pass" else "fail",
       boundary_records = 0L, sole_failures = sum(sole))
}
