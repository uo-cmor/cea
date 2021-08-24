#' Bootstrap Resampling of CEA Estimates
#'
#' Generate R bootstrap replicates of mean incremental QALYs and Costs from a
#'     fitted CEA regression model, using the `boot` package. Only the ordinary
#'     nonparametric bootstrap is currently implemented.
#'
#' @param fit `cea_estimate` object. The fitted CEA regression model. Must use
#'     the default 'formula' specification.
#' @param R The number of bootstrap replicates.
#' @param weights,parallel,ncpus,cl Passed to `\link[boot]{boot}`. For
#'     `parallel` and `ncpus`, default values are taken from
#'     `getOption(cea.boot.parallel)` and `getOption(cea.boot.ncpus)` instead
#'     of their `boot`-package equivalents.
#'
#' @export
boot <- function(fit, R, weights = NULL, simple = FALSE, parallel = c("no", "multicore", "snow"),
                 ncpus = getOption("cea.boot.ncpus", 1L), cl = NULL) {
  if (missing(parallel)) parallel <- getOption("cea.boot.parallel", "no")
  est_fun <- function(idxs, i) {
    call. <- attr(fit, "call")
    call.$data <- fit$data[i, ]
    fit_boot <- eval(call.)
    res <- cea_extract_estimate(fit_boot)
    c(QALYs = res$QALYs$effect, Costs = res$Costs$effect)
  }

  sink(tempfile())
  out <- boot::boot(seq_len(nrow(fit$data)), est_fun, R = R, weights = weights, simple = simple,
                    parallel = parallel, ncpus = ncpus, cl = cl)
  sink()

  class(out) <- c("cea_boot", class(out))
  out
}
