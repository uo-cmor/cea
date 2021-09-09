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
#' @param outcome String scalar. Name of the outcome measure to extract.
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

#' @rdname extract-effects
#' @export
extract <- function(x, outcome, estimand = "ATE") {
  if (!inherits(x, "cea_estimate")) stop_incorrect_class("cea_estimate")
  if (length(idx <- which(extract_outcomes(x) == outcome)) == 0) stop_unknown_outcome(outcome)
  tx <- attr(x, "tx")
  if (is.null(x$data[[tx]])) stop_unknown_treatment(tx)
  if (is.factor(x$data[[tx]])) tx <- paste0(tx, extract_tx(x))

  if (extract_link(x, idx) == "identity") {
    return(extract_tx_coefs(x, idx, tx))
  }

  coefs <- extract_coefs(x, idx)
  idx_tx <- extract_tx_idx(x, idx, tx)

  extract_effect <- function(i) {
    X <- extract_X(x, idx)
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
    linkinv <- stats::make.link(extract_link(x, idx))$linkinv

    mean(linkinv(X1 %*% coefs) - linkinv(X0 %*% coefs))
  }

  out <- vapply(idx_tx, extract_effect, numeric(1))
  names(out) <- extract_tx(x)
  out
}

extract_tx <- function(x) UseMethod("extract_tx")
extract_tx.cea_mcglm <- function(x) {
  if (!is.factor(x$data[[attr(x, "tx")]])) return("")

  tx_vars <- x$beta_names[[1]][grepl(attr(x, "tx"), x$beta_names[[1]])]
  sub(attr(x, "tx"), "", tx_vars)
}
extract_tx.cea_mglmmPQL <- function(x) {
  if (!is.factor(x$data[[attr(x, "tx")]])) return("")

  tx_vars <- names(x$coefficients$fixed)[grepl(attr(x, "tx"), names(x$coefficients$fixed))]
  unique(gsub(paste0(attr(x, "tx"), "|(:.*)"), "", tx_vars))
}

extract_outcomes <- function(x) UseMethod("extract_outcomes")
extract_outcomes.cea_mcglm <- function(x) names(x$linear_pred)
extract_outcomes.cea_mglmmPQL <- function(x) names(x$mvfixed)

extract_link <- function(x, idx) UseMethod("extract_link")
extract_link.cea_mcglm <- function(x, idx) x$link[[idx]]
extract_link.cea_mglmmPQL <- function(x, idx) getfamily(idx, x$family)$link

extract_coefs <- function(x, idx, ...) UseMethod("extract_coefs")
extract_coefs.cea_mcglm <- function(x, idx, nm = FALSE, ...) {
  if (idx == "reg") {
    out <- x$Regression
    if (nm) names(out) <- unlist(Map(function(lab, nms) paste0(lab, ".", nms),
                                     names(x$beta_names), x$beta_names))
  } else if (idx == "all") {
    out <- c(x$Regression, x$Covariance)
    if (nm) {
      names(out) <- c(
        unlist(Map(function(lab, nms) paste0(lab, ".", nms), names(x$beta_names), x$beta_names)),
        colnames(vcov(x))[(length(x$Regression) + 1):length(out)]
      )
    }
  } else{
    i <- Reduce(`+`, init = 0,
                x$Information$n_betas[seq_len(idx - 1)]) + seq_len(x$Information$n_betas[[idx]])
    out <- x$Regression[i]
    if (nm) names(out) <- x$beta_names[[idx]]
  }

  out
}
extract_coefs.cea_mglmmPQL <- function(x, idx, nm = TRUE, ...) {
  if (nm) x$coefficients$fixed else unname(x$coefficients$fixed)
}

extract_tx_coefs <- function(x, idx, tx) UseMethod("extract_tx_coefs")
extract_tx_coefs.cea_mcglm <- function(x, idx, tx) {
  stats::setNames(extract_coefs(x, idx)[which(x$beta_names[[idx]] %in% tx)], extract_tx(x))
}
extract_tx_coefs.cea_mglmmPQL <- function(x, idx, tx) {
  stats::setNames(extract_coefs(x, idx, TRUE)[paste0(tx, ":outvar", x$outcomes[[idx]])], extract_tx(x))
}

extract_tx_idx <- function(x, idx, tx) UseMethod("extract_tx_idx")
extract_tx_idx.cea_mcglm <- function(x, idx, tx) which(x$beta_names[[idx]] %in% tx)
extract_tx_idx.cea_mglmmPQL <- function(x, idx, tx) {
  which(names(extract_coefs(x, idx, TRUE)) %in% paste0(tx, ":outvar", x$outcomes[[idx]]))
}

extract_X <- function(x, idx) UseMethod("extract_X")
extract_X.cea_mcglm <- function(x, idx) x$list_X[[idx]]
extract_X.cea_mglmmPQL <- function(x, idx) {
  stats::model.matrix(x$terms, x$data.mglmmPQL[as.integer(x$data.mglmmPQL$outvar) == idx, ])
}
