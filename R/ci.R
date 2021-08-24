ci <- function(x, outcomes = "INMB", method = "boot", R, conf = 0.9, type = "bca", wtp, ...) {
  if (!inherits(x, "cea_estimate")) stop_not_cea_estimate()
  if (attr(x, "spec") != "formula") stop_not_formula_spec("ICER")
  if (!identical(method, "boot")) stop_unknown_method(method)
  if (!all(outcomes %in% c("QALYs", "Costs", "INMB", "INHB")))
    stop_unknown_outcome(outcomes[which.max(!(outcomes %in% c("QALYs", "Costs", "INMB", "INHB")))])
  if (any(c("INMB", "INHB") %in% outcomes) && missing(wtp)) stop_missing_wtp()
  if (method == "boot" && missing(R)) stop_missing_R()
  if (!(type %in% c("perc", "norm", "basic", "bca"))) stop_invalid_ci_type(type)
  if (type == "bca" & R < nrow(x$data)) stop_R_too_small(R, nrow(x$data))
  boot_est <- boot(x, R = R, ...)

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
  attr(out, "type") <- type
  attr(out, "R") <- R

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

  cat(conf, "% CONFIDENCE INTERVALS:\n", sep = "")
  cat("Based on", R, "bootstrap replicates\n")
  cat("Intervals calculated using", type, "method\n\n")
  print(unclass(x))

  invisible(y)
}
