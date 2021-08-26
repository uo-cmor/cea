#' Incremental Treatment EFfects
#'
#' Extract incremental treatment effects from a fitted `cea_estimate`
#'     regression object.
#'
#' @param x `cea_estimate` object. The fitted CEA regression model. Must
#'     contain a 'QALYs' equation.
#' @param wtp Numeric scalar. Willingness-to-pay per QALY gained.
#' @param estimand String scalar. Whether to calculate the average treatment
#'     effect (ATE), average treatment effect on the treated (ATT), or average
#'     treatment effect on the controls (ATC). Only used for non-linear models.
#'
#' @aliases QALYs Costs ICER INMB INHB
#'
#' @name extract-effects
NULL

#' @rdname extract-effects
#' @export
QALYs <- function(x, estimand = "ATE") {
  extract(x, "QALYs", estimand = estimand)
}

#' @rdname extract-effects
#' @export
Costs <- function(x, estimand = "ATE") {
  extract(x, "Costs", estimand = estimand)
}

#' @rdname extract-effects
#' @export
ICER <- function(x, estimand = "ATE") {
  Costs(x, estimand) / QALYs(x, estimand)
}

#' @rdname extract-effects
#' @export
INMB <- function(x, wtp, estimand = "ATE") {
  QALYs(x, estimand) * wtp - Costs(x, estimand)
}

#' @rdname extract-effects
#' @export
INHB <- function(x, wtp, estimand = "ATE") {
  QALYs(x, estimand) - Costs(x, estimand) / wtp
}

extract <- function(x, outcome, estimand = "ATE") {
  if (!inherits(x, "cea_estimate")) stop_not_cea_estimate()
  if (length(idx <- which(names(x$linear_pred) == outcome)) == 0) stop_unknown_outcome(outcome)
  tx <- attr(x, "tx")
  if (is.null(x$data[[tx]])) stop_unknown_treatment(tx)
  if (is.factor(x$data[[tx]])) tx <- paste0(tx, levels(x$data[[tx]])[[2]])
  idx_tx <- which(x$beta_names[[idx]] == tx)

  if (x$link[[idx]] == "identity") {
    idx_reg <- Reduce(`+`, x$Information$n_betas[seq_len(idx - 1)], 0) + idx_tx
    return(x$Regression[[idx_reg]])
  }

  idx_coef <-
    Reduce(`+`, x$Information$n_betas[seq_len(idx - 1)], 0) + seq_len(x$Information$n_betas[[idx]])
  coefs <- x$Regression[idx_coef]

  X0 <- X1 <- x$list_X[[idx]]
  X0[, idx_tx] <- 0
  X1[, idx_tx] <- 1
  if (estimand != "ATE") {
    tx_values <- x$list_X[[idx]][, tx]
    if (estimand == "ATT") {
      X0 <- X0[tx_values == 1, ]
      X1 <- X1[tx_values == 1, ]
    } else if (estimand == "ATC") {
      X0 <- X0[tx_values == 0, ]
      X1 <- X1[tx_values == 0, ]
    } else {
      stop_unknown_estimand(estimand)
    }
  }
  linkinv <- stats::make.link(x$link[[idx]])$linkinv

  mean(linkinv(X1 %*% coefs)) - mean(linkinv(X0 %*% coefs))
}
