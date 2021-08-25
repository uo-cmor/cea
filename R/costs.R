#' Incremental Intervention Costs
#'
#' Extract incremental costs from a fitted `cea_estimate` regression object.
#'
#' @param x `cea_estimate` object. The fitted CEA regression model. Must use
#'     the default 'formula' specification.
#' @param estimand String scalar. Whether to calculate INMB from the average
#'     treatment effect (ATE), average treatment effect on the treated (ATT),
#'     or average treatment effect on the controls (ATC).
#'
#' @family treatment effect extractors
#'
#' @export
Costs <- function(x, estimand = "ATE") {
  if (!inherits(x, "cea_estimate")) stop_not_cea_estimate()

  cost_var <- rlang::as_name(attr(stats::terms(x$linear_pred[[2]]), "variables")[[2]])
  tx_var <- rlang::as_name(attr(stats::terms(x$linear_pred[[2]]), "variables")[[3]])
  tx <- x$data[[tx_var]]
  if (is.factor(tx)) tx <- as.integer(tx) - 1
  coefs <- x$Regression[(length(x$Regression) / 2 + 1):length(x$Regression)]
  X0 <- X1 <- x$list_X$Costs
  X0[, 2] <- 0
  X1[, 2] <- 1
  if (estimand == "ATT") {
    X0 <- X0[tx == 1, ]
    X1 <- X1[tx == 1, ]
  } else if (estimand == "ATC") {
    X0 <- X0[tx == 0, ]
    X1 <- X1[tx == 0, ]
  } else if (estimand != "ATE") {
    stop_unknown_estimand(estimand)
  }
  mean(exp(X1 %*% coefs)) - mean(exp(X0 %*% coefs))
}
