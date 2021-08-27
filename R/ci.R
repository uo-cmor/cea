#' Confidence Intervals for CEA Estimates
#'
#' Generate confidence intervals for mean incremental QALYs, costs, net
#'     monetary benefit (INMB), and net health benefit (INHB) from a fitted CEA
#'      regression model.
#'
#' The 'boot' method uses `boot` and `boot.ci` from the `boot` package. The
#'     'delta' method uses the delta approximation (\cite{Oehlert 1992}).
#'
#' @references
#' Oehlert GW. \emph{A note on the delta method}. Am Stat 1992;46(1):27-9.
#'     \url{https://doi.org/10.2307/2684406}
#'
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
#' @param method Which method to use. Available methods are 'boot' (bootstrap)
#'     and 'delta' (delta approximation) are implemented.
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
  if (!rlang::is_string(method, c("boot", "delta"))) stop_unknown_method(method)
  if (!rlang::is_character(outcomes)) stop_invalid_outcome()
  if (!all(outcomes %in% c(names(x$linear_pred), "INMB", "INHB"))) stop_unknown_outcome(
    outcomes[which.max(!(outcomes %in% c(names(x$linear_pred), "INMB", "INHB")))]
  )
  if (any(c("INMB", "INHB") %in% outcomes) && missing(wtp)) stop_missing_wtp()

  if (method == "delta") {
    out <- calculate_delta_cis(x, outcomes, conf, wtp, estimand)
  } else {
    if (method == "boot" && missing(R)) stop_missing_R()
    if (method == "boot" && !rlang::is_string(type, c("perc", "norm", "basic", "bca")))
      stop_invalid_ci_type(type)
    if (method == "boot" && type == "bca" && R < nrow(x$data)) stop_R_too_small(R, nrow(x$data))
    if (method == "boot" && type == "bca" && sim == "parametric") stop_invalid_bca_parametric()

    boot_est <- boot(x, R = R, estimand = estimand, sim = sim, ...)

    out <- calculate_boot_cis(boot_est, outcomes, conf, type, wtp)
  }

  class(out) <- "cea_ci"
  attr(out, "conf") <- conf
  attr(out, "method") <- method
  if (method == "boot") attr(out, "type") <- type
  if (method == "boot") attr(out, "R") <- R
  if (method == "boot") attr(out, "sim") <- sim

  out
}

#' @rdname ci
#' @export
ci.cea_boot <- function(x, outcomes = "INMB", conf = 0.9, type = "bca", wtp, ...) {
  if (!all(outcomes %in% c(names(x$t0), "INMB", "INHB"))) stop_unknown_outcome(
    outcomes[which.max(!(outcomes %in% c(names(x$t0), "INMB", "INHB")))]
  )
  if (any(c("INMB", "INHB") %in% outcomes) && missing(wtp)) stop_missing_wtp()
  if (!(type %in% c("perc", "norm", "basic", "bca"))) stop_invalid_ci_type(type)
  if (type == "bca" & x$sim == "parametric") stop_invalid_bca_parametric()
  if (type == "bca" & x$R < length(x$data)) stop_R_too_small(x$R, length(x$data))

  out <- calculate_boot_cis(x, outcomes, conf, type, wtp)

  class(out) <- "cea_ci"
  attr(out, "conf") <- conf
  attr(out, "method") <- "boot"
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
  method <- attr(x, "method")
  attr(x, "method") <- NULL
  if (method == "boot") {
    type <- switch(attr(x, "type"),
                   perc = "bootstrap percentile", norm = "normal approximation",
                   basic = "basic bootstrap", bca = "adjusted bootstrap percentile (BCa)")
    attr(x, "type") <- NULL
    R <- attr(x, "R")
    attr(x, "R") <- NULL
    sim <- attr(x, "sim")
    attr(x, "sim") <- NULL
  }

  cat(conf, "% CONFIDENCE INTERVALS:\n", sep = "")
  if (method == "boot") {
    cat("Based on", R, sim, "bootstrap replicates\n")
    cat("Intervals calculated using", type, "method")
  } else if (method == "delta") {
    cat("Based on delta approximation")
  }
  cat("\n\n")
  print(unclass(x))

  invisible(y)
}

calculate_boot_cis <- function(x, outcomes, conf, type, wtp) {
  out <- list()

  if (any(c("INMB", "INHB", "QALYs") %in% outcomes)) idx.QALYs <- which(names(x$t0) == "QALYs")
  if (any(c("INMB", "INHB", "Costs") %in% outcomes)) idx.Costs <- which(names(x$t0) == "Costs")

  for (i in outcomes) {
    out[[i]] <- switch(
      i,
      QALYs = boot::boot.ci(x, conf = conf, type = type, index = idx.QALYs),
      Costs = boot::boot.ci(x, conf = conf, type = type, index = idx.Costs),
      INMB = boot::boot.ci(x, conf = conf, type = type,
                           t0 = rlang::set_names(x$t0["QALYs"] * wtp - x$t0["Costs"], "INMB"),
                           t = x$t[, idx.QALYs] * wtp - x$t[, idx.Costs]),
      INHB = boot::boot.ci(x, conf = conf, type = type,
                           t0 = rlang::set_names(x$t0["QALYs"] - x$t0["Costs"] / wtp, "INHB"),
                           t = x$t[, idx.QALYs] - x$t[, idx.Costs] / wtp),
      boot::boot.ci(x, conf = conf, type = type, index = which(names(x$t0) == i))
    )
    out[[i]] <- out[[i]][[4]][1, ncol(out[[i]][[4]]) - (1:0)]
  }
  out <- rlang::exec(rbind, !!!out)
  colnames(out) <- c("Lower", "Upper")
  out
}

calculate_delta_cis <- function(x, outcomes, conf, wtp, estimand) {
  V <- extract_var(x)
  out <- list()

  if (any(c("INMB", "INHB", "QALYs") %in% outcomes)) dmu.QALYs <- extract_dmu(x, "QALYs", estimand)
  if (any(c("INMB", "INHB", "Costs") %in% outcomes)) dmu.Costs <- extract_dmu(x, "Costs", estimand)

  for (i in outcomes) {
    out[[i]] <- switch(
      i,
      QALYs = c(QALYs(x, estimand), delta_se(dmu.QALYs, V)),
      Costs = c(Costs(x, estimand), delta_se(dmu.Costs, V)),
      INMB = c(INMB(x, wtp, estimand), delta_se(dmu.QALYs * wtp - dmu.Costs, V)),
      INHB = c(INHB(x, wtp, estimand), delta_se(dmu.QALYs - dmu.Costs / wtp, V)),
      c(extract(x, i, estimand), delta_se(extract_dmu(x, i, estimand), V))
    )
    out[[i]] <- out[[i]][[1]] + c(-1, 1) * stats::qnorm(1 - (1 - conf) / 2) * out[[i]][[2]]
  }
  out <- rlang::exec(rbind, !!!out)
  colnames(out) <- c("Lower", "Upper")
  out
}
