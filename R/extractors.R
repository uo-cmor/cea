#' Incremental treatment eFfects
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

# `extract()` should be made generic to allow different methods for `cea_estimate` subclasses
# Or define classed subfunctions (extract_tx_idxs, etc) to allow the same wrapper function to be
# used for different subclasses
extract <- function(x, outcome, estimand = "ATE") {
  if (!inherits(x, "cea_estimate")) stop_incorrect_class("cea_estimate")
  if (length(idx <- which(names(x$linear_pred) == outcome)) == 0) stop_unknown_outcome(outcome)
  tx <- attr(x, "tx")
  if (is.null(x$data[[tx]])) stop_unknown_treatment(tx)
  if (is.factor(x$data[[tx]])) tx <- paste0(tx, extract_tx(x))
  idx_tx <- which(x$beta_names[[idx]] %in% tx)

  if (x$link[[idx]] == "identity") {
    idx_reg <- Reduce(`+`, x$Information$n_betas[seq_len(idx - 1)], 0) + idx_tx
    out <- x$Regression[idx_reg]
    names(out) <- extract_tx(x)
    return(out)
  }

  idx_coef <-
    Reduce(`+`, x$Information$n_betas[seq_len(idx - 1)], 0) + seq_len(x$Information$n_betas[[idx]])
  coefs <- x$Regression[idx_coef]

  extract_effect <- function(i) {
    X <- x$list_X[[idx]]
    X[, setdiff(idx_tx, i)] <- 0
    X0 <- X1 <- X
    X0[, i] <- 0
    X1[, i] <- 1
    if (estimand != "ATE") {
      tx0 <- rowSums(X[, idx_tx, drop = FALSE]) == 0
      tx1 <- X[, i] == 1
      if (estimand == "ATT") {
        X0 <- X0[tx1, ]
        X1 <- X1[tx1, ]
      } else if (estimand == "ATC") {
        X0 <- X0[tx0, ]
        X1 <- X1[tx0, ]
      } else {
        stop_unknown_estimand(estimand)
      }
    }
    linkinv <- stats::make.link(x$link[[idx]])$linkinv

    mean(linkinv(X1 %*% coefs) - linkinv(X0 %*% coefs))
  }

  out <- vapply(idx_tx, extract_effect, numeric(1))
  names(out) <- extract_tx(x)
  out
}

extract_tx <- function(x) {
  if (!is.factor(x$data[[attr(x, "tx")]])) return("")

  tx_vars <- x$beta_names[[1]][grepl(attr(x, "tx"), x$beta_names[[1]])]
  sub(attr(x, "tx"), "", tx_vars)
}
