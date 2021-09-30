#' Pool multiply-imputed CEA regression results
#'
#' Combine estimates from `m` repeated complete data CEA regression analyses,
#'     using Rubin's pooling rules.
#'
#' @param x A `cea_mira` object. The fitted CEA regression models (on `m`
#'     imputed datasets).
#' @param ... For future expansion. Not currently used.
#'
#' @export
pool_cea <- function(x, ...) {
  if (!inherits(x, "cea_mira")) stop_incorrect_class("cea_mira")

  cl <- match.call()
  x1 <- x$analyses[[1]]

  coefs <- lapply(x$analyses, function(z) extract_coefs(z, "all", TRUE))
  vcovs <- lapply(x$analyses, function(z) as.matrix(vcov(z)))

  m <- length(coefs)
  Qbar <- Reduce(`+`, coefs) / m
  Ubar <- Reduce(`+`, vcovs) / m
  B <- Reduce(function(accum, q) accum + tcrossprod(q - Qbar), coefs,
              init = tcrossprod(Qbar - Qbar)) / (m - 1)
  t <- Ubar + (1 + 1 / m) * B

  b <- diag(B)
  ubar <- diag(Ubar)
  dfcom <- extract_df(x1)
  df <- barnard.rubin(m, b, t, dfcom)
  riv <- (1 + 1 / m) * b / ubar
  lambda <- (1 + (1 / m)) * b / diag(t)
  fmi <- (riv + 2 / (df + 3)) / (riv + 1)

  pooled <- data.frame(estimate = Qbar, ubar = diag(Ubar), b = b, t = diag(t),
                       dfcom = dfcom, df = df, riv = riv, lambda = lambda, fmi = fmi,
                       row.names = names(Qbar))

  make_pooled(x, pooled, t, m)
}

#' @export
print.cea_mcglm_pooled <- function(x, ...) {
  cat("======================================================\n")
  cat("=== Pooled Cost-Effectiveness Regression Estimates ===\n")
  cat("======================================================\n")

  cat("Based on", x$m, "imputed datasets.\n\n")

  cat("Call:\n", rlang::quo_text(attr(x, "call")), "\n\n")

  cat("Data:\n", rlang::quo_text(attr(x, "call1")), "\n\n")

  cat("------------------\n")
  cat("Univariate Models:\n\n")
  for (i in seq_along(x$linear_pred)) {
    nm <- names(x$linear_pred)[[i]]
    len_nm <- nchar(nm, type = "width")
    form <- deparse(x$linear_pred[[i]], width.cutoff = 80 - len_nm - 6)

    cat("  ", nm, ": ",
        paste(form, collapse = paste0("\n", strrep(" ", len_nm + 2))), "\n", sep = "")
    cat("    * Link function:", x$link[[i]], "\n")
    cat("    * Variance function:", x$variance[[i]], "\n")
    cat("    * Covariance function:", x$covariance[[i]], "\n\n")
  }

  cat("------------------\n")
  cat("Incremental Treatment Effects:\n")
  if (is_factor_tx(x)) {
    cat("        ", pad(extract_tx(x), 10), "\n")
  }
  cat("  QALYs:", sprintf("%+10.3f", QALYs(x)), "\n")
  cat("  Costs:", sprintf("%+10.0f", Costs(x)), "\n")
  cat("  ICER: ", sprintf("%10.0f", ICER(x)), "\n\n")

  cat("===============================================\n")

  return(invisible(x))
}

#' @export
print.cea_mglmmPQL_pooled <- function(x, ...) {
  cat("======================================================\n")
  cat("=== Pooled Cost-Effectiveness Regression Estimates ===\n")
  cat("======================================================\n")

  cat("Based on", x$m, "imputed datasets.\n\n")

  cat("Call:\n", rlang::quo_text(attr(x, "call")), "\n\n")

  cat("Data:\n", rlang::quo_text(attr(x, "call1")), "\n\n")

  cat("------------------\n")
  cat("Univariate Models:\n\n")
  for (i in seq_along(x$mvfixed)) {
    nm <- names(x$mvfixed)[[i]]
    len_nm <- nchar(nm, type = "width")
    x$mvfixed[[i]][[2]] <- rlang::sym(levels(x$data.mglmmPQL$outvar)[[i]])
    form <- deparse(x$mvfixed[[i]], width.cutoff = 80 - len_nm - 6)

    cat("  ", nm, ": ",
        paste(form, collapse = paste0("\n", strrep(" ", len_nm + 2))), "\n", sep = "")
    cat("    * Family:", get_family(i, x$family)$family, "\n")
    cat("    * Link:", get_family(i, x$family)$link, "\n")
  }

  cat("------------------\n")
  cat("Incremental Treatment Effects:\n")
  if (is_factor_tx(x)) {
    cat("        ", pad(extract_tx(x), 10), "\n")
  }
  cat("  QALYs:", sprintf("%+10.3f", QALYs(x)), "\n")
  cat("  Costs:", sprintf("%+10.0f", Costs(x)), "\n")
  cat("  ICER: ", sprintf("%10.0f", ICER(x)), "\n\n")

  cat("===============================================\n")

  return(invisible(x))
}

barnard.rubin <- function(m, b, t, dfcom = .Machine$double.xmax) {
  lambda <- (1 + (1 / m)) * b / diag(t)
  lambda[lambda < .Machine$double.eps] <- .Machine$double.eps
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)

  dfold * dfobs / (dfold + dfobs)
}

#' @importFrom stats coef
#' @export
coef.cea_mcglm_pooled <- function(object, ...) c(object$Regression, object$Covariance)

#' @importFrom stats vcov
#' @export
vcov.cea_mcglm_pooled <- function(object, ...) as.matrix(object$vcov)

extract_df <- function(x) UseMethod("extract_df")
extract_df.cea_mcglm <- function(x) length(x$residual) - Reduce(`+`, x$Information$n_beta)
extract_df.cea_mglmmPQL <- function(x) mean(x$fixDF$X)

make_pooled <- function(x, pooled, t, m) UseMethod("make_pooled", x$analyses[[1]])
make_pooled.cea_mcglm <- function(x, pooled, t, m) {
  x1 <- x$analyses[[1]]
  list_X <- Reduce(
    function(z0, z) lapply(seq_along(z$list_X), function(i) z0[[i]] + z$list_X[[i]] / m),
    x$analyses, init = lapply(x1$list_X, function(z) {z[] <- 0; z})
  )

  structure(
    list(m = m,
         Regression = pooled$estimate[seq_along(x1$Regression)],
         Covariance = pooled$estimate[length(x1$Regression) + seq_along(x1$Covariance)],
         vcov = t,
         Information = x1$Information,
         beta_names = x1$beta_names,
         power_fixed = x1$power_fixed,
         n_obs = x1$n_obs,
         link = x1$link,
         variance = x1$variance,
         covariance = x1$covariance,
         linear_pred = x1$linear_pred,
         list_X = list_X,
         matrix_pred = x1$matrix_pred,
         Ntrial = x1$Ntrial,
         offset = x1$offset,
         data = x1$data, ## This is only the first imputed dataset
         pooled = pooled),
    class = c("cea_mcglm_pooled", "cea_pooled", "cea_mcglm", "cea_estimate"),
    call = x$call, call1 = x$call1,
    tx = attr(x1, "tx")
  )
}
make_pooled.cea_mglmmPQL <- function(x, pooled, t, m) {
  x1 <- x$analyses[[1]]

  structure(
    list(m = m,
         dims = x1$dims,
         contrasts = x1$contrasts,
         coefficients = list(fixed = stats::setNames(pooled$estimate, rownames(pooled))),
         varFix = t,
         apVar = x1$apVar,
         logLik = NA_real_,
         numIter = x1$numIter,
         groups = x1$groups,
         call = x1$call,
         terms = x1$terms,
         method = x1$method,
         fixDF = x1$fitDF,
         na.action = x1$na.action,
         data = x1$data,
         family = x1$family,
         iter = vapply(x$analyses, function(z) z$iter, integer(1)),
         data.mglmmPQL = x1$data.mglmmPQL,
         mvfixed = x1$mvfixed,
         pooled = pooled),
    class = c("cea_mglmmPQL_pooled", "cea_pooled", "cea_mglmmPQL", "cea_estimate"),
    call = x$call, call1 = x$call1,
    tx = attr(x1, "tx")
  )
}

vcov.cea_mglmmPQL <- function(object, ...) object$varFix
