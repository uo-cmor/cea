#' Incremental treatment effects
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
#' @param draw Only used for `brms` models. Optional (numeric) draw index to
#'     extract. Default (`draw = NULL`) is to use the posterior mean.
#'
#' @aliases QALYs Costs ICER INMB INHB
#'
#' @name extract-effects
NULL

#' @rdname extract-effects
#' @export
QALYs <- function(x, estimand = "ATE", draw = NULL) {
  extract(x, "QALYs", estimand = estimand, draw = draw)
}

#' @rdname extract-effects
#' @export
Costs <- function(x, estimand = "ATE", draw = NULL) {
  extract(x, "Costs", estimand = estimand, draw = draw)
}

#' @rdname extract-effects
#' @export
ICER <- function(x, estimand = "ATE", draw = NULL) {
  Costs(x, estimand, draw = draw) / QALYs(x, estimand, draw = draw)
}

#' @rdname extract-effects
#' @export
INMB <- function(x, wtp, estimand = "ATE", draw = NULL) {
  QALYs(x, estimand, draw = draw) * wtp - Costs(x, estimand, draw = draw)
}

#' @rdname extract-effects
#' @export
INHB <- function(x, wtp, estimand = "ATE", draw = NULL) {
  QALYs(x, estimand, draw = draw) - Costs(x, estimand, draw = draw) / wtp
}

#' @rdname extract-effects
#' @export
extract <- function(x, outcome, estimand = "ATE", draw = NULL) {
  if (!inherits(x, "cea_estimate")) stop_incorrect_class("cea_estimate")
  if (length(idx <- which(extract_outcomes(x) == outcome)) == 0) stop_unknown_outcome(outcome)
  tx <- attr(x, "tx")
  if (is.null(x$data[[tx]])) stop_unknown_treatment(tx)

  if (extract_link(x, idx) == "identity") {
    return(extract_tx_coefs(x, idx, tx, draw = draw))
  }

  coefs <- extract_coefs(x, idx, draw = draw)
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

    colMeans(linkinv(X1 %*% coefs) - linkinv(X0 %*% coefs))
  }

  out <- sapply(idx_tx, extract_effect)
  if (is.array(out)) colnames(out) <- extract_tx(x) else names(out) <- extract_tx(x)
  out
}

extract_tx <- function(x) {
  if (!is.factor(x$data[[attr(x, "tx")]])) return("")
  UseMethod("extract_tx")
}
extract_tx.cea_mcglm <- function(x) {
  vars <- unique(unlist(x$beta_names, use.names = FALSE))
  tx_vars <- grep(paste0("^", attr(x, "tx")), vars, value = TRUE)
  sub(attr(x, "tx"), "", tx_vars)
}
extract_tx.cea_mglmmPQL <- function(x) {
  vars <- names(x$coefficients$fixed)
  tx_vars <- grep(paste0("^", attr(x, "tx")), vars, value = TRUE)
  unique(gsub(paste0(attr(x, "tx"), "|(:.*)"), "", tx_vars))
}
extract_tx.cea_brms <- function(x) {
  vars <- unique(x$prior$coef)
  tx_vars <- grep(paste0("^", attr(x, "tx")), vars, value = TRUE)
  sub(attr(x, "tx"), "", tx_vars)
}

extract_outcomes <- function(x) UseMethod("extract_outcomes")
extract_outcomes.cea_mcglm <- function(x) names(x$linear_pred)
extract_outcomes.cea_mglmmPQL <- function(x) names(x$mvfixed)
extract_outcomes.cea_brms <- function(x) names(x$family)

extract_link <- function(x, idx) UseMethod("extract_link")
extract_link.cea_mcglm <- function(x, idx) x$link[[idx]]
extract_link.cea_mglmmPQL <- function(x, idx) get_family(idx, x$family)$link
extract_link.cea_brms <- function(x, idx) x$family[[idx]]$link

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
  coefs <- x$coefficients$fixed
  if (!missing(idx) && is.numeric(idx))
    coefs <- coefs[grepl(paste0("outvar", levels(x$data.mglmmPQL$outvar)[[idx]]), names(coefs))]
  if (nm) coefs else unname(coefs)
}
extract_coefs.cea_brms <- function(x, idx, nm = TRUE, draw = NULL, ...) {
  coefs <- if (is.null(draw)) {
    cc <- brms::fixef(x)[, "Estimate"]
    dim(cc) <- c(length(cc), 1)
    dimnames(cc) <- list(rownames(brms::fixef(x)), NULL)
    cc
  } else {
    cc <- as.matrix(x, draw = draw)
    cc <- cc[, grepl("^b_", colnames(cc)), drop = FALSE]
    colnames(cc) <- sub("^b_", "", colnames(cc))
    t(cc)
  }
  if (!missing(idx) && is.numeric(idx))
    coefs <- coefs[grepl(paste0("^", names(x$formula$forms)[[idx]], "_"), rownames(coefs)), , drop = FALSE]
  if (nm) coefs else unname(coefs)
}

extract_tx_coefs <- function(x, idx, tx, ...) {
  cc <- extract_coefs(x, idx, ...)
  if (is.null(dim(cc))) {
    stats::setNames(cc[extract_tx_idx(x, idx, tx)], extract_tx(x))
  } else {
    cc <- t(cc[extract_tx_idx(x, idx, tx), , drop = FALSE])
    colnames(cc) <- extract_tx(x)
    cc
  }
}

extract_tx_idx <- function(x, idx, tx) UseMethod("extract_tx_idx")
extract_tx_idx.cea_mcglm <- function(x, idx, tx) grep(paste0("^", tx), x$beta_names[[idx]])
extract_tx_idx.cea_mglmmPQL <- function(x, idx, tx) {
  if (length(levels(x$data.mglmmPQL$outvar)) == 1) {
    grep(paste0("^", tx), names(extract_coefs(x, idx)))
  } else {
    grep(paste0("^", tx, ".*:outvar", levels(x$data.mglmmPQL$outvar)[[idx]]),
         names(extract_coefs(x, idx)))
  }
}
extract_tx_idx.cea_brms <- function(x, idx, tx) {
  grep(paste0("^", names(x$formula$forms)[[idx]], "_", tx), rownames(extract_coefs(x, idx)))
}

extract_X <- function(x, idx) UseMethod("extract_X")
extract_X.cea_mcglm <- function(x, idx) x$list_X[[idx]]
extract_X.cea_mglmmPQL <- function(x, idx) stats::model.matrix(x$mvfixed[[idx]], x$data)
extract_X.cea_brms <- function(x, idx) stats::model.matrix(x$formula$forms[[idx]]$formula, x$data)
