#' Confidence Intervals for CEA Estimates
#'
#' Generate confidence intervals for mean incremental QALYs, costs, net
#'     monetary benefit (INMB), and net health benefit (INHB) from a fitted CEA
#'      regression model.
#' @param x `cea_estimate` or `cea_boot` object. The fitted CEA regression
#'     model or bootstrap resampling from the fitted model.
#' @param estimand String scalar. Whether to calculate the average treatment
#'     effect (ATE), average treatment effect on the treated (ATT), or average
#'     treatment effect on the controls (ATC). Only used for non-linear models.
#' @param outcomes A character vector indicating the outcomes to be calculated.
#'     Possible values are "QALYs", "Costs", "INMB", or "INHB".
#' @param conf Confidence level of the required intervals.
#' @param type Which type of intervals are required. Possible values are
#'     "norm", "basic", "perc", or "bca".
#' @param wtp Willingness-to-pay level for calculation of INMB & INHB.
#' @param method Which method to use. Currently only 'boot' (bootstrap) is
#'     implemented.
#' @param R The number of bootstrap replicates.
#' @param sim A character vector indicating the type of simulation required.
#'     Possible values are "ordinary" (the default), "parametric", "balanced",
#'     or "permutation".
#' @param ... Passed to \code{\link{boot}}.
#'
#' @export
ci <- function(x, outcomes = "INMB", conf = 0.9, type = "bca", wtp, estimand = "ATE", ...) {
  UseMethod("ci")
}

#' @rdname ci
#' @export
ci.cea_estimate <- function(x, outcomes = "INMB", conf = 0.9, type = "bca", wtp, estimand = "ATE",
                            method = "boot", R, sim = "ordinary", ...) {
  if (!identical(method, "boot")) stop_unknown_method(method)
  if (!all(outcomes %in% c("QALYs", "Costs", "INMB", "INHB")))
    stop_unknown_outcome(outcomes[which.max(!(outcomes %in% c("QALYs", "Costs", "INMB", "INHB")))])
  if (any(c("INMB", "INHB") %in% outcomes) && missing(wtp)) stop_missing_wtp()
  if (method == "boot" && missing(R)) stop_missing_R()
  if (!(type %in% c("perc", "norm", "basic", "bca"))) stop_invalid_ci_type(type)
  if (type == "bca" & R < nrow(x$data)) stop_R_too_small(R, nrow(x$data))
  if (type == "bca" & sim == "parametric") stop_invalid_bca_parametric()

  boot_est <- boot(x, R = R, estimand = estimand, sim = sim, ...)

  out <- list()
  for (i in outcomes) {
    out[[i]] <- switch(
      i,
      QALYs = boot::boot.ci(boot_est, conf = conf, type = type, index = 1),
      Costs = boot::boot.ci(boot_est, conf = conf, type = type, index = 2),
      INMB = boot::boot.ci(boot_est, conf = conf, type = type,
                           t0 = rlang::set_names(boot_est$t0[1] * wtp - boot_est$t0[2], "INMB"),
                           t = boot_est$t[, 1] * wtp - boot_est$t[, 2]),
      INHB = boot::boot.ci(boot_est, conf = conf, type = type,
                           t0 = rlang::set_names(boot_est$t0[1] - boot_est$t0[2] / wtp, "INHB"),
                           t = boot_est$t[, 1] - boot_est$t[, 2] / wtp)
    )
    out[[i]] <- out[[i]][[4]][1, ncol(out[[i]][[4]]) - (1:0)]
  }

  out <- rlang::exec(rbind, !!!out)
  colnames(out) <- c("Lower", "Upper")
  class(out) <- "cea_ci"
  attr(out, "conf") <- conf
  if (method == "boot") attr(out, "type") <- type
  if (method == "boot") attr(out, "R") <- R
  if (method == "boot") attr(out, "sim") <- sim

  out
}

#' @rdname ci
#' @export
ci.cea_boot <- function(x, outcomes = "INMB", conf = 0.9, type = "bca", wtp, ...) {
  if (!all(outcomes %in% c("QALYs", "Costs", "INMB", "INHB")))
    stop_unknown_outcome(outcomes[which.max(!(outcomes %in% c("QALYs", "Costs", "INMB", "INHB")))])
  if (any(c("INMB", "INHB") %in% outcomes) && missing(wtp)) stop_missing_wtp()
  if (!(type %in% c("perc", "norm", "basic", "bca"))) stop_invalid_ci_type(type)
  if (type == "bca" & x$sim == "parametric") stop_invalid_bca_parametric()
  if (type == "bca" & x$R < length(x$data)) stop_R_too_small(x$R, length(x$data))

  out <- list()
  for (i in outcomes) {
    out[[i]] <- switch(
      i,
      QALYs = boot::boot.ci(x, conf = conf, type = type, index = 1),
      Costs = boot::boot.ci(x, conf = conf, type = type, index = 2),
      INMB = boot::boot.ci(x, conf = conf, type = type,
                           t0 = rlang::set_names(x$t0[1] * wtp - x$t0[2], "INMB"),
                           t = x$t[, 1] * wtp - x$t[, 2]),
      INHB = boot::boot.ci(x, conf = conf, type = type,
                           t0 = rlang::set_names(x$t0[1] - x$t0[2] / wtp, "INHB"),
                           t = x$t[, 1] - x$t[, 2] / wtp)
    )
    out[[i]] <- out[[i]][[4]][1, ncol(out[[i]][[4]]) - (1:0)]
  }

  out <- rlang::exec(rbind, !!!out)
  colnames(out) <- c("Lower", "Upper")
  class(out) <- "cea_ci"
  attr(out, "conf") <- conf
  attr(out, "type") <- type
  attr(out, "R") <- x$R
  attr(out, "sim") <- x$sim

  out
}

#' @export
print.cea_ci <- function(x, ...) {
  ops <- options(scipen = 5)
  on.exit(options(ops), add = TRUE)
  y <- x

  conf <- attr(x, "conf") * 100
  attr(x, "conf") <- NULL
  type <- switch(attr(x, "type"),
                 perc = "bootstrap percentile", norm = "normal approximation",
                 basic = "basic bootstrap", bca = "adjusted bootstrap percentile (BCa)")
  attr(x, "type") <- NULL
  R <- attr(x, "R")
  attr(x, "R") <- NULL
  sim <- attr(x, "sim")
  attr(x, "sim") <- NULL

  cat(conf, "% CONFIDENCE INTERVALS:\n", sep = "")
  cat("Based on", R, sim, "bootstrap replicates\n")
  cat("Intervals calculated using", type, "method\n\n")
  print(unclass(x))

  invisible(y)
}
