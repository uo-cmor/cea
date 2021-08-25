#' Incremental Quality-Adjusted Life Year Gains
#'
#' Extract incremental QALY gains from a fitted `cea_estimate` regression
#'     object.
#'
#' @param x `cea_estimate` object. The fitted CEA regression model. Must use
#'     the default 'formula' specification.
#'
#' @family treatment effect extractors
#'
#' @export
QALYs <- function(x) {
  if (!inherits(x, "cea_estimate")) stop_not_cea_estimate()
  x$Regression[2]
}
