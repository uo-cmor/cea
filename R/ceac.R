#' Cost-Effectiveness Acceptability Curves
#'
#' Generate a cost-effectiveness acceptability curve (CEAC) from a fitted CEA
#'      regression model.
#'
#' @param x `cea_estimate` or `cea_boot` object. The fitted CEA regression
#'     model or bootstrap resampling from the fitted model.
#' @param wtp_max Maximum willingness-to-pay level for calculation.
#' @param wtp_step Interval between calculated willingness-to-pay levels.
#' @param QALYs,Costs Names of the variables in `x` representing QALYs and
#'     Costs, respectively.
#' @param estimand String scalar. Whether to calculate the average treatment
#'     effect (ATE), average treatment effect on the treated (ATT), or average
#'     treatment effect on the controls (ATC). Only used for non-linear models.
#' @param method Which method to use. Currently only 'boot' (bootstrap) is
#'     implemented.
#' @param R The number of bootstrap replicates.
#' @param sim A character vector indicating the type of simulation required.
#'     Possible values are "ordinary" (the default), "parametric", "balanced",
#'     or "permutation".
#' @param ... Passed to \code{\link{boot_cea}}.
#'
#' @export
ceac <- function(x, wtp_max, wtp_step, QALYs = "QALYs", Costs = "Costs", ...) {
  UseMethod("ceac")
}

#' @rdname ceac
#' @export
ceac.cea_estimate <- function(x, wtp_max, wtp_step, QALYs = "QALYs", Costs = "Costs",
                              estimand = "ATE", method = "delta", R, sim = "parametric", ...) {
  if (!inherits(x, "cea_brms") && !rlang::is_string(method, c("boot", "delta")))
    stop_unknown_method(method)
  if (inherits(x, "cea_brms") && (!missing(method) || !missing(R) || !missing(sim)))
    message_unused_brms_arguments("ceac")
  if (method == "boot" && missing(R)) stop_missing_R()
  if (!all(c(QALYs, Costs) %in% extract_outcomes(x)))
    stop_unknown_outcome(c(QALYs, Costs)[which.max(!(c(QALYs, Costs) %in% extract_outcomes(x)))])

  wtp <- seq.int(0, wtp_max, wtp_step)
  n_tx <- length(extract_tx(x))

  if (inherits(x, "cea_brms")) {
    extract_ceac <- calculate_posterior_ceac(x, wtp, QALYs, Costs, estimand)
  } else if (method == "delta") {
    extract_ceac <- calculate_delta_ceac(x, wtp, QALYs, Costs, estimand)
  } else if (method == "boot") {
    boot_est <- boot_cea(x, R = R, estimand = estimand, sim = sim, ...)
    extract_ceac <- calculate_boot_ceac(boot_est, wtp, QALYs, Costs)
  }

  ceac <- lapply(seq_len(n_tx), extract_ceac)
  out <- tibble::tibble(wtp = rep(wtp, n_tx), ceac = unlist(ceac))
  if (is_factor_tx(x)) out$tx = rep(extract_tx(x), each = length(wtp))

  class(out) <- c("cea_ceac", class(out))
  attr(out, "method") <- method
  if (method == "boot") attr(out, "R") <- R
  if (method == "boot") attr(out, "sim") <- sim

  out
}

#' @rdname ceac
#' @export
ceac.cea_boot <- function(x, wtp_max, wtp_step, QALYs = "QALYs", Costs = "Costs", ...) {
  mult_tx <- is.matrix(x$t0)
  nm <- if (mult_tx) colnames(x$t0) else names(x$t0)
  if (!all(c(QALYs, Costs) %in% nm))
    stop_unknown_outcome(c(QALYs, Costs)[which.max(!(c(QALYs, Costs) %in% nm))])

  wtp <- seq.int(0, wtp_max, wtp_step)
  n_tx <- nrow(x$t0) %||% 1

  extract_ceac <- calculate_boot_ceac(x, wtp, QALYs, Costs)

  ceac <- lapply(seq_len(n_tx), extract_ceac)
  out <- tibble::tibble(wtp = rep(wtp, n_tx), ceac = unlist(ceac))
  if (is_factor_tx(x)) out$tx = rep(attr(x, "tx"), each = length(wtp))

  class(out) <- c("cea_ceac", class(out))
  attr(out, "method") <- "boot"
  attr(out, "R") <- x$R
  attr(out, "sim") <- x$sim

  out
}

#' @export
autoplot.cea_ceac <- function(object, wtp = NULL, ...) {
  out <- if (("tx" %in% names(object))) {
    ggplot2::ggplot(object,
                    ggplot2::aes(.data$wtp, .data$ceac, colour = .data$tx, linetype = .data$tx))
  } else ggplot2::ggplot(object, ggplot2::aes(.data$wtp, .data$ceac))
  if (!is.null(wtp)) out <- out + ggplot2::geom_vline(xintercept = wtp, colour = "red", alpha = 0.5)
  out <- out +
    ggplot2::geom_line() +
    ggplot2::xlab("Willingness-to-pay threshold") + ggplot2::ylab("CEAC") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                                limits = c(0, 1)) +
    ggplot2::scale_x_continuous(labels = scales::label_dollar())

  if (("tx" %in% names(object))) out <- out +
    ggplot2::scale_color_brewer("Treatment group", type = "qual", palette = 2) +
    ggplot2::scale_linetype_discrete("Treatment group")

  out
}

#' @export
plot.cea_ceac <- function(x, ...) {
  print(autoplot(x, ...))
}

calculate_boot_ceac <- function(x, wtp, QALYs = "QALYs", Costs = "Costs", ...) {
  idx.QALYs <- which((names(x$t0) %||% colnames(x$t0)[slice.index(x$t0, 2)]) == QALYs)
  idx.Costs <- which((names(x$t0) %||% colnames(x$t0)[slice.index(x$t0, 2)]) == Costs)
  force(x)
  force(wtp)
  function(j) {
    colMeans((x$t[, c(idx.QALYs[[j]], idx.Costs[[j]])] %*% rbind(wtp, -1)) > 0)
  }
}

calculate_delta_ceac <- function(x, wtp, QALYs, Costs, estimand, ...) {
  V <- extract_var(x)
  dmu.QALYs <- extract_dmu(x, QALYs, estimand)
  dmu.Costs <- extract_dmu(x, Costs, estimand)
  function(j) {
    vapply(
      wtp,
      function(a) stats::pnorm(0, INMB(x, a, estimand)[[j]],
                               delta_se(dmu.QALYs[[j]] * a - dmu.Costs[[j]], V),
                               lower.tail = FALSE),
      numeric(1)
    )
  }
}

calculate_posterior_ceac <- function(x, wtp, QALYs, Costs, estimand, ...) {
  nd <- brms::ndraws(x)
  function(j)
    vapply(wtp, function(a) mean(INMB(x, a, estimand, draw = seq_len(nd))[, j] > 0), numeric(1L))
}
