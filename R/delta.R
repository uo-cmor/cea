extract_dmu <- function(x, outcome, estimand = "ATE") {
  if (!inherits(x, "cea_estimate")) stop_incorrect_class("cea_estimate")
  if (length(idx <- which(extract_outcomes(x) == outcome)) == 0) stop_unknown_outcome(outcome)
  tx <- attr(x, "tx")
  if (is.null(x$data[[tx]])) stop_unknown_treatment(tx)

  coefs <- extract_coefs(x, idx)
  idx_tx <- extract_tx_idx(x, idx, tx)

  idx_coef <- if (inherits(x, "cea_mcglm")) {
    Reduce(`+`, x$Information$n_betas[seq_len(idx - 1)], 0) + seq_along(coefs)
  } else if (inherits(x, "cea_mglmmPQL")) {
    grep(paste0("outvar", levels(x$data.mglmmPQL$outvar)[[idx]]),
         names(extract_coefs(x, "all")))
  }

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
    mu.eta <- stats::make.link(extract_link(x, idx))$mu.eta

    d <- (crossprod(mu.eta(X1 %*% coefs), X1) - crossprod(mu.eta(X0 %*% coefs), X0)) / nrow(X1)
    out <- matrix(rep(0, ncol(extract_var(x))), nrow = 1)
    out[idx_coef] <- d
    out
  }
  out <- lapply(idx_tx, extract_effect)
  names(out) <- extract_tx(x)
  out
}

extract_var <- function(x) UseMethod("extract_var")
extract_var.cea_mcglm <- function(x) {
  V <- as.matrix(vcov(x))
  i <- extract_nbeta(x)
  V[1:i, 1:i]
}
extract_var.cea_mglmmPQL <- function(x) as.matrix(vcov(x))

delta_se <- function(G, V) c(sqrt(G %*% tcrossprod(V, G)))

# G <- extract_dmu(x, outcome, estimand)
# V <- extract_var(x)
