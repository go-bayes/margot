#' Boilerplate provider helpers (JSON/list/function)
#'
#' Allows external, JSON-based or function-provided text to override or augment
#' internal explanations. Configure via options:
#' - `options(margot.boilerplate = <list|function|filepath>)`
#' - `options(margot.boilerplate.labels = <named list of default labels>)`
#' - `options(margot.boilerplate.acronyms = <named list of acronym expansions>)`
#'
#' Slot examples:
#' - policy_value_explainer: character or list(policy=..., research=...)
#' - methods_long / methods_short / methods_prereg: glue templates
#'
#' @keywords internal
margot_load_boilerplate <- function() {
  prov <- getOption("margot.boilerplate", NULL)
  if (is.null(prov)) return(NULL)
  # list provided
  if (is.list(prov)) return(prov)
  # function provider
  if (is.function(prov)) {
    out <- tryCatch(prov(), error = function(e) NULL)
    if (is.list(out)) return(out)
    return(NULL)
  }
  # path to JSON
  if (is.character(prov) && length(prov) == 1 && file.exists(prov)) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      out <- tryCatch(jsonlite::read_json(prov, simplifyVector = TRUE), error = function(e) NULL)
      if (is.list(out)) return(out)
    }
  }
  NULL
}

#' Get boilerplate slot text
#' @keywords internal
margot_get_boilerplate <- function(slot, audience = NULL, context = list(), default = NULL) {
  db <- margot_load_boilerplate()
  if (is.null(db)) return(default)
  val <- db[[slot]]
  if (is.null(val)) return(default)
  # audience-specific dispatch
  if (!is.null(audience) && is.list(val) && audience %in% names(val)) {
    val <- val[[audience]]
  }
  if (!is.character(val) || !length(val)) return(default)
  # interpolate with glue if possible
  out <- tryCatch(glue::glue(val, .envir = list2env(context, parent = emptyenv())), error = function(e) val)
  as.character(out)
}

