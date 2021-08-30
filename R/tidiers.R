#' @importFrom generics tidy
#' @export
generics::tidy

#' @export
tidy.cea_estimate <- function(x, ...) {
  summ <- with_sink(tempfile(), summary(x))
  vars <- names(x$beta_names)
  extract_tidy <- function(x, comp, lab) {
    out <- tibble::as_tibble(x[[comp]], rownames = "term")
    out$y.level <- lab
    out
  }

  corr <- tibble::as_tibble(summ$Correlation)
  reg <- lapply(seq_along(vars), function(i) extract_tidy(summ[[i]], "Regression", vars[[i]]))
  tau <- lapply(seq_along(vars), function(i) extract_tidy(summ[[i]], "tau", vars[[i]]))

  n_beta <- vapply(reg, nrow, integer(1))
  reg <- rlang::exec(rbind, !!!reg)
  colnames(reg) <- c("term", "estimate", "std.error", "statistic", "p.value", "y.level")
  reg$component <- "regression"

  n_tau <- vapply(tau, nrow, integer(1))
  tau <- rlang::exec(rbind, !!!tau)
  colnames(tau) <- c("term", "estimate", "std.error", "statistic", "p.value", "y.level")
  tau$term = paste0("tau", rep(seq_along(vars), n_tau), tau$term)
  tau$component = "dispersion"

  corr$y.level <- NA_character_
  colnames(corr) <- c("term", "estimate", "std.error", "statistic", "p.value", "y.level")
  corr$component <- "correlation"

  out <- rbind(reg, corr, tau)
  cols <- c("component", "y.level", names(out)[1:5])
  out[] <- out[, cols]
  colnames(out) <- cols
  out
}
