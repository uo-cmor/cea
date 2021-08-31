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

#' @export
tidy.cea_pooled <- function(x, ...) {
  vars <- names(x$beta_names)
  nvars <- length(vars)
  n_beta <- unlist(x$Information$n_betas)
  n_rho <- x$Information$n_rho
  n_tau <- unlist(x$Information$n_taus)

  idxs_beta <- split(seq_along(x$Regression), rep(seq_along(n_beta), n_beta))
  idxs_rho <- seq_len(n_rho) + sum(n_beta)
  idxs_tau <- split(seq_along(x$Covariance[-seq_along(idxs_rho)]) + n_rho + sum(n_beta),
                    rep(seq_along(n_tau), n_tau))

  coefs <- coef(x)
  vcov <- vcov(x)

  extract_tidy_beta <- function(idx, term, lab) {
    tibble::tibble(component = "regression",
                   y.level = lab,
                   term = term,
                   estimate = coefs[idx],
                   std.error = sqrt(vcov[cbind(idx, idx)]),
                   statistic = .data$estimate / .data$std.error,
                   p.value = 2 * stats::pnorm(abs(.data$statistic), lower.tail = FALSE))
  }
  extract_tidy_rho <- function(idx) {
    tibble::tibble(
      component = "correlation",
      y.level = NA_character_,
      term = paste0("rho",
                    rep(seq_len(nvars - 1), rev(seq_len(nvars - 1))),
                    sequence(rev(seq_len(nvars - 1)), seq_len(nvars)[-1])),
      estimate = coefs[idx],
      std.error = sqrt(vcov[cbind(idx, idx)]),
      statistic = .data$estimate / .data$std.error,
      p.value = 2 * stats::pnorm(abs(.data$statistic), lower.tail = FALSE)
    )
  }
  extract_tidy_tau <- function(idx, i, lab) {
    tibble::tibble(component = "dispersion",
                   y.level = lab,
                   term = paste0("tau", i, seq_along(idx)),
                   estimate = coefs[idx],
                   std.error = sqrt(vcov[idx, idx]),
                   statistic = .data$estimate / .data$std.error,
                   p.value = 2 * stats::pnorm(abs(.data$statistic), lower.tail = FALSE))
  }

  reg <- lapply(seq_along(vars),
                function(i) extract_tidy_beta(idxs_beta[[i]], x$beta_names[[i]], vars[[i]]))
  rho <- extract_tidy_rho(idxs_rho)
  tau <- lapply(seq_along(vars),
                function(i) extract_tidy_tau(idxs_tau[[i]], i, vars[[i]]))

  reg <- rlang::exec(rbind, !!!reg)
  tau <- rlang::exec(rbind, !!!tau)

  rbind(reg, rho, tau)
}
