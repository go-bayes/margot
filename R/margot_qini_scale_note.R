#' Generate Explanatory Note for QINI Scale
#'
#' Creates a descriptive note explaining what the QINI curve scale represents
#' based on the selected scale type (average, cumulative, or population).
#'
#' @param scale Character string specifying the scale: "average", "cumulative", or "population"
#' @param n_units Optional integer specifying the number of units (for population scale)
#'
#' @return A character string containing the explanatory note
#' @export
#' @examples
#' margot_qini_scale_note("average")
#' margot_qini_scale_note("cumulative")
#' margot_qini_scale_note("population", n_units = 1000)
margot_qini_scale_note <- function(scale = "average", n_units = NULL) {
  scale <- match.arg(scale, c("average", "cumulative", "population"))

  if (scale == "average") {
    return(paste0(
      "The y-axis shows average policy effects per unit (Q(B) = E[⟨πB(Xi), τ(Xi)⟩]), ",
      "representing the expected gain from treating units according to the policy. ",
      "This is the standard maq implementation where gains converge to the ATE at 100% spend."
    ))
  } else if (scale == "cumulative") {
    return(paste0(
      "The y-axis shows cumulative gains, where the gain at each point represents ",
      "the total accumulated benefit from treating all units up to that proportion. ",
      "This is the traditional uplift modeling approach where curves show total impact."
    ))
  } else if (scale == "population") {
    if (!is.null(n_units)) {
      return(paste0(
        "The y-axis shows total population impact for ", n_units, " units. ",
        "Values represent the absolute gain in the outcome from treating units ",
        "according to the prioritization policy."
      ))
    } else {
      return(paste0(
        "The y-axis shows total population impact. Values represent the absolute ",
        "gain in the outcome from treating units according to the prioritization policy."
      ))
    }
  }
}

#' Generate Brief Scale Description for Plot Subtitles
#'
#' Creates a brief description of the QINI scale for use in plot subtitles
#'
#' @param scale Character string specifying the scale: "average", "cumulative", or "population"
#' @param n_units Optional integer specifying the number of units (for population scale)
#'
#' @return A character string containing the brief description
#' @export
#' @examples
#' margot_qini_scale_subtitle("average")
#' margot_qini_scale_subtitle("cumulative")
#' margot_qini_scale_subtitle("population", n_units = 1000)
margot_qini_scale_subtitle <- function(scale = "average", n_units = NULL) {
  scale <- match.arg(scale, c("average", "cumulative", "population"))

  if (scale == "average") {
    return("Showing average policy effects (maq default)")
  } else if (scale == "cumulative") {
    return("Showing cumulative gains")
  } else if (scale == "population") {
    if (!is.null(n_units)) {
      return(paste0("Showing total impact for ", n_units, " units"))
    } else {
      return("Showing total population impact")
    }
  }
}
