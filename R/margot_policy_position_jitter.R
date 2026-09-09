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

# place a separator at the inclusive band's edge, with symmetric jitter for all rows.
.margot_policy_band_layout <- function(x, cut, width, upper_closed = TRUE) {
  x <- x[is.finite(x)]
  below <- if (upper_closed) x <= cut else x < cut
  if (!any(below) || all(below) || width == 0) {
    return(list(width = 0, boundary = cut))
  }
  lower <- max(x[below])
  upper <- min(x[!below])
  width <- min(width, 0.49 * (upper - lower))
  list(width = width, boundary = if (upper_closed) lower + width else upper - width)
}

# resolve band geometry after scale transformation for matching point and line layers.
.margot_policy_band_position <- function(x, cut, width, height, seed, line = FALSE) {
  if (any(!is.finite(c(width, height))) || any(c(width, height) < 0)) {
    stop("jitter widths and heights must be finite non-negative numbers", call. = FALSE)
  }
  ggplot2::ggproto(NULL, ggplot2::PositionJitter,
    width = width, height = height, seed = seed,
    reference_x = x, cut = cut, line = line,
    required_aes = if (line) character() else c("x", "y"),
    setup_params = function(self, data) {
      params <- ggplot2::position_jitter(width = self$width,
        height = self$height, seed = self$seed)$setup_params(data)
      params$reference_x <- self$reference_x
      params$cut <- self$cut
      params$line <- self$line
      params
    },
    compute_panel = function(data, params, scales) {
      scale <- scales$x
      inverse <- scale$get_transformation()$inverse
      layout <- .margot_policy_band_layout(scale$transform(params$reference_x),
        scale$transform(params$cut), params$width, !isTRUE(inverse(1) < inverse(0)))
      if (params$line) {
        data$xintercept <- layout$boundary
        return(data)
      }
      params$width <- layout$width
      ggplot2::PositionJitter$compute_panel(data, params, scales)
    }
  )
}
