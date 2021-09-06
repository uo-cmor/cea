#' Confidence intervals for CEA estimates
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
#' @param ... Passed to \code{\link{boot_cea}}.
#'
#' @export
ci <- function(x, outcomes = "INMB", conf = 0.9, type = "bca", wtp, estimand = "ATE", ...) {
  UseMethod("ci")
}

#' @rdname ci
#' @export
ci.cea_estimate <- function(x, outcomes = "INMB", conf = 0.9, type = "bca", wtp, estimand = "ATE",
                            method = "delta", R, sim = "parametric", ...) {
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

    boot_est <- boot_cea(x, R = R, estimand = estimand, sim = sim, ...)

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
  mult_tx <- is.matrix(x$t0)
  nm <- if (mult_tx) colnames(x$t0) else names(x$t0)
  if (!all(outcomes %in% c(nm, "INMB", "INHB")))
    stop_unknown_outcome(outcomes[which.max(!(outcomes %in% c(nm, "INMB", "INHB")))])
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
  cat("\n")
  for (i in seq_along(x)) {
    cat("\n", names(x)[[i]], ":\n", sep = "")
    print(x[[i]])
  }
  # print(unclass(x))

  invisible(y)
}

calculate_boot_cis <- function(x, outcomes, conf, type, wtp) {
  out <- list()

  if (any(c("INMB", "INHB", "QALYs") %in% outcomes))
    idx.QALYs <- which((names(x$t0) %||% colnames(x$t0)[slice.index(x$t0, 2)]) == "QALYs")
  if (any(c("INMB", "INHB", "Costs") %in% outcomes))
    idx.Costs <- which((names(x$t0) %||% colnames(x$t0)[slice.index(x$t0, 2)]) == "Costs")

  for (i in outcomes) {
    out[[i]] <- switch(
      i,
      QALYs = lapply(idx.QALYs, function(j) boot::boot.ci(x, conf = conf, type = type, index = j)),
      Costs = lapply(idx.Costs, function(j) boot::boot.ci(x, conf = conf, type = type, index = j)),
      INMB = lapply(
        seq_along(idx.QALYs),
        function(j) boot::boot.ci(
          x, conf = conf, type = type,
          t0 = rlang::set_names(x$t0["QALYs"] * wtp - x$t0["Costs"], "INMB"),
          t = x$t[, idx.QALYs[[j]]] * wtp - x$t[, idx.Costs[[j]]]
        )
      ),
      INHB = lapply(
        seq_along(idx.QALYs),
        function(j) boot::boot.ci(
          x, conf = conf, type = type,
          t0 = rlang::set_names(x$t0["QALYs"] - x$t0["Costs"] / wtp, "INHB"),
          t = x$t[, idx.QALYs[[j]]] - x$t[, idx.Costs[[j]]] / wtp
        )
      ),
      {
        idx <- which(names(x$t0) %||% colnames(x$t0)[slice.index(x$t0, 2)] == i)
        lapply(idx, function(j) boot::boot.ci(x, conf = conf, type = type, index = j))
      }
    )
    out[[i]] <- t(vapply(out[[i]], function(z) z[[4]][1, ncol(z[[4]]) - (1:0)], numeric(2)))
    colnames(out[[i]]) <- c("Lower", "Upper")
    rownames(out[[i]]) <- attr(x, "tx")
  }
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
      QALYs = lapply(seq_along(dmu.QALYs),
                     function(j) c(QALYs(x, estimand)[[j]], delta_se(dmu.QALYs[[j]], V))),
      Costs = lapply(seq_along(dmu.Costs),
                     function(j) c(Costs(x, estimand)[[j]], delta_se(dmu.Costs[[j]], V))),
      INMB = lapply(
        seq_along(dmu.QALYs),
        function(j) c(INMB(x, wtp, estimand)[[j]], delta_se(dmu.QALYs[[j]] * wtp - dmu.Costs[[j]], V))
      ),
      INHB = lapply(
        seq_along(dmu.QALYs),
        function(j) c(INHB(x, wtp, estimand)[[j]], delta_se(dmu.QALYs[[j]] - dmu.Costs[[j]] / wtp, V))
      ),
      lapply(extract_dmu(x, i, estimand),
             function(j) c(extract(x, i, estimand)[[j]], delta_se(j, V)))
    )
    out[[i]] <- t(vapply(out[[i]],
                         function(z) z[[1]] + c(-1, 1) * stats::qnorm(1 - (1 - conf) / 2) * z[[2]],
                         numeric(2)))
    colnames(out[[i]]) <- c("Lower", "Upper")
    rownames(out[[i]]) <- extract_tx(x)
  }
  out
}
