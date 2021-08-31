extract_dmu <- function(x, outcome, estimand = "ATE") {
  if (!inherits(x, "cea_estimate")) stop_incorrect_class("cea_estimate")
  if (length(idx <- which(names(x$linear_pred) == outcome)) == 0) stop_unknown_outcome(outcome)
  tx <- attr(x, "tx")
  if (is.null(x$data[[tx]])) stop_unknown_treatment(tx)
  if (is.factor(x$data[[tx]])) tx <- paste0(tx, extract_tx(x))
  idx_tx <- which(x$beta_names[[idx]] %in% tx)

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
    mu.eta <- stats::make.link(x$link[[idx]])$mu.eta

    d <- (crossprod(mu.eta(X1 %*% coefs), X1) - crossprod(mu.eta(X0 %*% coefs), X0)) / nrow(X1)
    out <- matrix(rep(0, length(x$Regression) + length(x$Covariance)), nrow = 1)
    out[idx_coef] <- d
    out
  }
  out <- lapply(idx_tx, extract_effect)
  names(out) <- extract_tx(x)
  out
}

extract_var <- function(x) as.matrix(x$vcov)

delta_se <- function(G, V) c(sqrt(G %*% tcrossprod(V, G)))

# G <- extract_dmu(x, outcome, estimand)
# V <- extract_var(x)
