#' Incremental Cost-Effectiveness Ratio
#'
#' Calculate the incremental cost-effectiveness ratio (ICER) from a fitted
#'     `cea_estimate` object.
#'
#' @param x `cea_estimate` object. The fitted CEA regression model. Must use
#'     the default 'formula' specification.
#' @param estimand String scalar. Whether to calculate ICER from the average
#'     treatment effect (ATE), average treatment effect on the treated (ATT),
#'     or average treatment effect on the controls (ATC).
#' @param ... Not used.
#'
#' @family treatment effect extractors
#'
#' @export
ICER <- function(x, estimand = "ATE", ...) {
  if (!inherits(x, "cea_estimate")) stop_not_cea_estimate()
  Costs(x, estimand) / QALYs(x)
}
