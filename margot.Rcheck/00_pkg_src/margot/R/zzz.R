# package announcement
.onAttach <- function(libname, pkgname) {
  # respect quiet option
  if (isTRUE(getOption("margot.quiet"))) {
    return(invisible())
  }

  version <- utils::packageDescription(pkgname, fields = "Version")
  packageStartupMessage("margot ", version)
}

