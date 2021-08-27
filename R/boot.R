#' Bootstrap Resampling of CEA Estimates
#'
#' Generate R bootstrap replicates of mean incremental QALYs and Costs from a
#'     fitted CEA regression model, using the `boot` package.
#'
#' @param x `cea_estimate` object. The fitted CEA regression model. Must use
#'     the default 'formula' specification.
#' @param R The number of bootstrap replicates.
#' @param estimand String scalar. Whether to calculate the average treatment
#'     effect (ATE), average treatment effect on the treated (ATT), or average
#'     treatment effect on the controls (ATC). Only used for non-linear models.
#' @param sim A character vector indicating the type of simulation required.
#'     Possible values are "ordinary" (the default), "parametric", "balanced",
#'     or "permutation".
#' @param weights,simple,parallel,ncpus,cl Passed to `\link[boot]{boot}`. For
#'     `parallel` and `ncpus`, default values are taken from
#'     `getOption(cea.boot.parallel)` and `getOption(cea.boot.ncpus)` instead
#'     of their `boot`-package equivalents.
#'
#' @export
boot <- function(x, R, estimand = "ATE", sim = "ordinary", weights = NULL,
                 simple = FALSE, parallel = c("no", "multicore", "snow"),
                 ncpus = getOption("cea.boot.ncpus", 1L), cl = NULL) {
  if (!inherits(x, "cea_estimate")) stop_not_cea_estimate()
  if (!rlang::is_string(sim, c("ordinary", "parametric", "balanced", "permutation")))
    stop_unknown_sim(sim)
  if (missing(parallel)) parallel <- getOption("cea.boot.parallel", "no")
  outcomes <- names(x$linear_pred)
  if (sim == "parametric") {
    par_fun <- function(data) {
      vapply(outcomes, extract, numeric(1), x = data, estimand = estimand)
    }
    ran_fun <- function(data, mle) {
      out <- data
      nbeta <- Reduce(`+`, out$Information$n_betas)
      rand <- c(mvtnorm::rmvnorm(1, c(out$Regression, out$Covariance), as.matrix(out$vcov)))
      out$Regression <- rand[1:nbeta]
      out$Covariance <- rand[(nbeta + 1):length(rand)]
      out
    }
    out <- eval(rlang::expr(boot::boot(
      x, par_fun, R = !!R, sim = "parametric", weights = !!weights, ran.gen = ran_fun,
      simple = !!simple, parallel = !!parallel, ncpus = !!ncpus, cl = !!cl
    )))
  } else {
    est_fun <- function(idxs, i) {
      call. <- attr(x, "call")
      call.$data <- x$data[i, ]
      fit_boot <- eval(call.)
      vapply(outcomes, extract, numeric(1), x = fit_boot, estimand = estimand)
    }
    out <- eval(rlang::expr(boot::boot(
      seq_len(!!nrow(x$data)), est_fun, R = !!R, sim = !!sim, weights = !!weights,
      simple = !!simple, parallel = !!parallel, ncpus = !!ncpus, cl = !!cl
    )))
  }

  class(out) <- c("cea_boot", class(out))
  out
}

#' @export
autoplot.cea_boot <- function(object, wtp = NULL, QALYs = "QALYs", Costs = "Costs", ...) {
  if (!all(c(QALYs, Costs) %in% names(object$t0)))
    stop_unknown_outcome(c(QALYs, Costs)[which.max(!(c(QALYs, Costs) %in% names(object$t0)))])

  plotdata <- tibble::as_tibble(object$t, .name_repair = ~names(object$t0))

  out <- ggplot2::ggplot(plotdata, ggplot2::aes(.data[[QALYs]], .data[[Costs]])) +
    ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0)
  if (!is.null(wtp)) out <- out + ggplot2::geom_abline(slope = wtp, colour = "red", alpha = 0.5)
  out <- out +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(object$t0[[!!QALYs]], object$t0[[!!Costs]]), size = 3, colour = "red") +
    ggplot2::xlab("Incremental QALYs") + ggplot2::ylab("Incremental Costs") +
    ggplot2::scale_y_continuous(labels = scales::label_dollar())

  out
}

#' @export
plot.cea_boot <- function(x, ...) {
  print(autoplot(x, ...))
}
