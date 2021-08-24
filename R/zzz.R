.onLoad <- function(libname, pkgname) {
  op <- options()
  op.cea <- list(
    cea.boot.ncpus = max(1, parallel::detectCores() - 1),
    cea.boot.parallel = "no"
  )
  toset <- !(names(op.cea) %in% names(op))
  if (any(toset)) options(op.cea[toset])

  invisible()
}
