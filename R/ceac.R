#' Cost-Effectiveness Acceptability Curves
#'
#' Generate a cost-effectiveness acceptability curve (CEAC) from a fitted CEA
#'      regression model.
#' @param x `cea_estimate` object. The fitted CEA regression model. Must use
#'     the default 'formula' specification.
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
#' @param ... Passed to \code{\link{boot}}.
#'
#' @export
ceac <- function(x, wtp_max, wtp_step, QALYs = "QALYs", Costs = "Costs", ...) {
  UseMethod("ceac")
}

#' @rdname ceac
#' @export
ceac.cea_estimate <- function(x, wtp_max, wtp_step, QALYs = "QALYs", Costs = "Costs",
                              estimand = "ATE", method = "boot", R, sim = "ordinary", ...) {
  if (!rlang::is_string(method, c("boot", "delta"))) stop_unknown_method(method)
  if (method == "boot" && missing(R)) stop_missing_R()
  if (!all(c(QALYs, Costs) %in% names(x$linear_pred)))
    stop_unknown_outcome(c(QALYs, Costs)[which.max(!(c(QALYs, Costs) %in% names(x$linear_pred)))])

  wtp <- seq.int(0, wtp_max, wtp_step)

  if (method == "delta") {
    V <- extract_var(x)
    dmu.QALYs <- extract_dmu(x, QALYs, estimand)
    dmu.Costs <- extract_dmu(x, Costs, estimand)
    ceac <- vapply(
      wtp,
      function(a) stats::pnorm(0, INMB(x, a, estimand), delta_se(dmu.QALYs * a - dmu.Costs, V),
                               lower.tail = FALSE),
      numeric(1)
    )
    out <- tibble::tibble(wtp = wtp, ceac = ceac)
  } else if (method == "boot") {
    boot_est <- boot(x, R = R, estimand = estimand, sim = sim, ...)

    idxs <- c(which(names(boot_est$t0) == QALYs), which(names(boot_est$t0) == Costs))

    out <- tibble::tibble(
      wtp = wtp,
      ceac = colMeans((boot_est$t[, idxs] %*% rbind(wtp, -1)) > 0)
    )
  }

  class(out) <- c("cea_ceac", class(out))
  attr(out, "method") <- method
  if (method == "boot") attr(out, "R") <- R
  if (method == "boot") attr(out, "sim") <- sim

  out
}

#' @rdname ceac
#' @export
ceac.cea_boot <- function(x, wtp_max, wtp_step, QALYs = "QALYs", Costs = "Costs", ...) {
  if (!all(c(QALYs, Costs) %in% names(x$t0)))
    stop_unknown_outcome(c(QALYs, Costs)[which.max(!(c(QALYs, Costs) %in% names(x$t0)))])

  wtp <- seq.int(0, wtp_max, wtp_step)
  idxs <- c(which(names(x$t0) == QALYs), which(names(x$t0) == Costs))

  out <- tibble::tibble(
    wtp = wtp,
    ceac = colMeans((x$t[, idxs] %*% rbind(wtp, -1)) > 0)
  )

  class(out) <- c("cea_ceac", class(out))
  attr(out, "method") <- "boot"
  attr(out, "R") <- x$R
  attr(out, "sim") <- x$sim

  out
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.cea_ceac <- function(object, wtp = NULL, ...) {
  out <- ggplot2::ggplot(object, ggplot2::aes(.data$wtp, .data$ceac))
  if (!is.null(wtp)) out <- out + ggplot2::geom_vline(xintercept = wtp, colour = "red", alpha = 0.5)
  out <- out +
    ggplot2::geom_line() +
    ggplot2::xlab("Willingness-to-pay threshold") + ggplot2::ylab("CEAC") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                                limits = c(0, 1)) +
    ggplot2::scale_x_continuous(labels = scales::label_dollar())

  out
}

#' @export
plot.cea_ceac <- function(x, ...) {
  print(autoplot(x, ...))
}
