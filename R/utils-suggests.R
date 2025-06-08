#' Check if a suggested package is installed
#'
#' Internal function to check if a package from Suggests is available
#' and provide helpful error messages if not.
#'
#' @param pkg Character string naming the package to check
#' @param fun Character string naming the function that requires the package (optional)
#' @param purpose Character string describing what the package is needed for (optional)
#'
#' @return Invisible TRUE if package is available, otherwise throws an error
#' @keywords internal
#' @noRd
check_suggests <- function(pkg, fun = NULL, purpose = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    fun_msg <- if (!is.null(fun)) paste0(" for ", fun, "()") else ""
    purpose_msg <- if (!is.null(purpose)) paste0(" (", purpose, ")") else ""
    
    # determine which bundle would provide this package
    bundle_msg <- ""
    if (pkg %in% c("grf", "lmtp", "policytree", "maq", "SuperLearner", 
                   "clarify", "EValue", "WeightIt", "MatchIt", "MatchThem")) {
      bundle_msg <- c("i" = "For all estimation packages: {.code install.packages('margot.models')}")
    } else if (pkg %in% c("ggplot2", "patchwork", "ggokabeito", "ggeffects", "DiagrammeR")) {
      bundle_msg <- c("i" = "For all visualisation packages: {.code install.packages('margot.viz')}")
    } else if (pkg %in% c("gt", "gtsummary", "flextable", "kableExtra", "knitr", 
                          "report", "parameters", "table1")) {
      bundle_msg <- c("i" = "For all reporting packages: {.code install.packages('margot.report')}")
    }
    
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required{fun_msg}{purpose_msg}.",
      "i" = "Install it with: {.code install.packages('{pkg}')}",
      bundle_msg
    ))
  }
  invisible(TRUE)
}

#' Check if multiple suggested packages are installed
#'
#' Internal function to check if multiple packages from Suggests are available.
#'
#' @param pkgs Character vector of package names to check
#' @param fun Character string naming the function that requires the packages (optional)
#' @param purpose Character string describing what the packages are needed for (optional)
#'
#' @return Invisible TRUE if all packages are available, otherwise throws an error
#' @keywords internal
#' @noRd
check_suggests_multiple <- function(pkgs, fun = NULL, purpose = NULL) {
  missing_pkgs <- character()
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, pkg)
    }
  }
  
  if (length(missing_pkgs) > 0) {
    fun_msg <- if (!is.null(fun)) paste0(" for ", fun, "()") else ""
    purpose_msg <- if (!is.null(purpose)) paste0(" (", purpose, ")") else ""
    
    cli::cli_abort(c(
      "The following packages are required{fun_msg}{purpose_msg}:",
      paste0("  ", missing_pkgs),
      "i" = "Install them with: {.code install.packages(c({paste0('\"', missing_pkgs, '\"', collapse = ', ')}))}"
    ))
  }
  invisible(TRUE)
}

# Internal helper functions for checking package availability
# No documentation needed as these are not exported

has_grf <- function() {
  requireNamespace("grf", quietly = TRUE)
}

has_lmtp <- function() {
  requireNamespace("lmtp", quietly = TRUE)
}

has_ggplot2 <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}

has_gt <- function() {
  requireNamespace("gt", quietly = TRUE)
}

has_models <- function() {
  has_grf() && has_lmtp()
}

has_viz <- function() {
  has_ggplot2() && requireNamespace("patchwork", quietly = TRUE)
}

has_report <- function() {
  has_gt() || requireNamespace("flextable", quietly = TRUE)
}