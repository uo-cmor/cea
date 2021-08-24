#' Incremental Net Monetary Benefit
#'
#' Calculate the incremental net monetary benefit (INMB) or incremental net
#'     health benefit (INHB) from a fitted `cea_estimate` object.
#'
#' @param x `cea_estimate` object. Fitted CEA regression model.
#' @param wtp Numeric scalar.
#' @param estimand String scalar. Whether to calculate INMB from the average
#'     treatment effect (ATE), average treatment effect on the treated (ATT),
#'     or average treatment effect on the controls (ATC).
#' @param ... Not used.
#'
#' @aliases INHB
#'
#' @export
INMB <- function(x, wtp, estimand = "ATE", ...) {
  if (!inherits(x, "cea_estimate")) stop_not_cea_estimate()
  if (attr(x, "spec") != "formula") stop_not_formula_spec("INMB")
  object <- cea_extract_estimate(x, estimand)
  object$QALYs$effect * wtp - object$Costs$effect
}

#' @rdname INMB
#' @export
INHB <- function(x, wtp, estimand = "ATE", ...) {
  if (!inherits(x, "cea_estimate")) stop_not_cea_estimate()
  if (attr(x, "spec") != "formula") stop_not_formula_spec("INHB")
  object <- cea_extract_estimate(x, estimand)
  object$QALYs$effect - object$Costs$effect / wtp
}
