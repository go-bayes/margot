# keep jittered coordinates inside the split interval containing each original value.
.margot_jitter_within_splits <- function(x, shifted, width, cuts, upper_closed = TRUE) {
  if (!length(cuts) || width == 0) return(shifted)
  cuts <- sort(unique(cuts[is.finite(cuts)]))
  lower <- rep(-Inf, length(x))
  upper <- rep(Inf, length(x))
  for (cut in cuts) {
    below <- if (upper_closed) x <= cut else x < cut
    below[is.na(below)] <- FALSE
    lower <- ifelse(!below, pmax(lower, cut), lower)
    upper <- ifelse(below, pmin(upper, cut), upper)
  }
  lo <- pmax(x - width, lower)
  hi <- pmin(x + width, upper)
  # retain strict inequalities even at the endpoints of the random-number range.
  if (upper_closed) {
    lo <- ifelse(is.finite(lower) & lo == lower,
      pmin(x, lower + .Machine$double.eps * pmax(1, abs(lower))), lo)
  } else {
    hi <- ifelse(is.finite(upper) & hi == upper,
      pmax(x, upper - .Machine$double.eps * pmax(1, abs(upper))), hi)
  }
  u <- pmax(0, pmin(1, (shifted - x + width) / (2 * width)))
  lo + u * (hi - lo)
}

# jitter in plotted coordinates, with transformed split bounds and a scoped seed.
.margot_policy_position_jitter <- function(width, height, seed, method,
                                           x_splits = numeric(), y_splits = numeric()) {
  method <- match.arg(method, c("standard", "within_splits"))
  if (method == "standard") {
    return(ggplot2::position_jitter(width = width, height = height, seed = seed))
  }
  for (value in list(width, height)) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
      stop("jitter widths and heights must be finite non-negative numbers", call. = FALSE)
    }
  }
  ggplot2::ggproto(NULL, ggplot2::PositionJitter,
    width = width, height = height, seed = seed,
    x_splits = x_splits, y_splits = y_splits,
    setup_params = function(self, data) {
      params <- ggplot2::position_jitter(width = self$width,
        height = self$height, seed = self$seed)$setup_params(data)
      params$x_splits <- self$x_splits
      params$y_splits <- self$y_splits
      params
    },
    compute_panel = function(data, params, scales) {
      shifted <- ggplot2::PositionJitter$compute_panel(data, params, scales)
      for (axis in c("x", "y")) {
        cuts <- params[[paste0(axis, "_splits")]]
        if (!length(cuts)) next
        scale <- scales[[axis]]
        inverse <- scale$get_transformation()$inverse
        upper_closed <- !isTRUE(inverse(1) < inverse(0))
        shifted[[axis]] <- .margot_jitter_within_splits(
          data[[axis]], shifted[[axis]],
          if (axis == "x") params$width else params$height,
          scale$transform(cuts), upper_closed
        )
      }
      shifted
    }
  )
}
