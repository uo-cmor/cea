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

  coefs <- lapply(
    x$analyses,
    function(z) {
      tz <- tidy(z)
      stats::setNames(
        tz$estimate, ifelse(tz$component == "regression", paste0(tz$y.level, ".", tz$term), tz$term)
      )
    }
  )
  vcovs <- lapply(x$analyses, function(z) as.matrix(z$vcov))

  m <- length(coefs)
  Qbar <- Reduce(`+`, coefs) / m
  Ubar <- Reduce(`+`, vcovs) / m
  B <- Reduce(function(accum, q) accum + tcrossprod(q - Qbar), coefs,
              init = tcrossprod(Qbar - Qbar)) / (m - 1)
  t <- Ubar + (1 + 1 / m) * B

  rank <- max(unlist(Map(`+`, x1$Information$n_betas, x1$Information$n_taus))) # to be reviewed!!!

  b <- diag(B)
  ubar <- diag(Ubar)
  dfcom <- x1$n_obs - rank # ???
  df <- barnard.rubin(m, b, t, dfcom)
  riv <- (1 + 1 / m) * b / ubar
  lambda <- (1 + (1 / m)) * b / diag(t)
  fmi <- (riv + 2 / (df + 3)) / (riv + 1)

  pooled <- data.frame(estimate = Qbar, ubar = diag(Ubar), b = b, t = diag(t),
                       dfcom = dfcom, df = df, riv = riv, lambda = lambda, fmi = fmi,
                       row.names = names(Qbar))

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
         data = x$analyses[[1]]$data, ## This is only the first imputed dataset
         pooled = pooled),
    class = c("cea_pooled", "cea_estimate"),
    call = x$call, call1 = x$call1,
    tx = attr(x1, "tx")
  )

  # extract lists of coefficients & vcov matrices (from each imputed dataset)
  # calculate pooled result (as in pool_vector in MOA-II project)
  # recreate into `cea_pooled / cea_estimate` object
}

#' @export
print.cea_pooled <- function(x, ...) {
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

barnard.rubin <- function(m, b, t, dfcom = .Machine$double.xmax) {
  lambda <- (1 + (1 / m)) * b / diag(t)
  lambda[lambda < .Machine$double.eps] <- .Machine$double.eps
  dfold <- (m - 1) / lambda^2
  dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)

  dfold * dfobs / (dfold + dfobs)
}

#' @importFrom stats coef
#' @export
coef.cea_pooled <- function(object, ...) c(object$Regression, object$Covariance)

#' @importFrom stats vcov
#' @export
vcov.cea_pooled <- function(object, ...) as.matrix(object$vcov)
