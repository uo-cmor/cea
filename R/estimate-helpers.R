make_random_spec <- function(data, method, n, cluster = NULL) {
  switch(
    method,
    mcglm = rep(
      list(c(
        mcglm::mc_id(data),
        if (!is.null(cluster)) mcglm::mc_mixed(stats::reformulate(cluster, intercept = FALSE), data)
      )),
      n
    ),
    mglmmPQL = if (!is.null(cluster)) {
      stats::as.formula(paste0("~ outvar - 1 | ", cluster))
    } else if (n > 1) {
      ~ outvar - 1 | .cons
    } else ~ 1 | .cons
  )
}

get_family <- function(j, family) {
  fm <- family[[j]]
  if (is.character(fm)) fm <- get(fm, mode = "function", envir = parent.frame())
  if (is.function(fm)) fm <- fm()
  if (is.null(fm$family)) {
    stop_invalid_family(j)
  }

  fm
}

get_link <- function(family) vapply(family, function(x) x$link, character(1))

extract_variance <- function(family) {
  vapply(
    seq_along(family),
    function(i) {
      if (family[[i]]$family == "quasi") {
        if (family[[i]]$varfun == "constant") "constant"
        else if (family[[i]]$varfun %in% c("mu", "mu^2", "mu^3")) "tweedie"
        else if (family[[i]]$varfun == "mu(1-mu)") "binomialP"
        else stop_unknown_variance(i)
      } else if (family[[i]]$family == "gaussian") {
        "constant"
      } else if (family[[i]]$family %in% c("poisson", "quasipoisson", "Gamma",
                                           "inverse.gaussian")) {
        "tweedie"
      } else if (family[[i]]$family %in% c("binomial", "quasibinomial")) {
        "binomialP"
      } else if (
        rlang::is_string(family[[i]]$variance)
        && family[[i]]$variance %in% c("constant", "tweedie", "poisson_tweedie", "binomialP",
                                       "binomialPQ")
      ) {
        family[[i]]$variance
      } else stop_unknown_variance(i)
    },
    character(1)
  )
}

extract_power <- function(family) {
  lapply(
    family,
    function(x) {
      if (x$family == "quasi") {
        if (x$varfun == "constant") 0
        else if (x$varfun == "mu^2") 2
        else if (x$varfun == "mu^3") 3
        else 1
      }
      else if (x$family == "gaussian") 0
      else if (x$family == "Gamma") 2
      else if (x$family == "inverse.gaussian") 3
      else if (rlang::is_string(x$variance) && x$variance == "tweedie") {
        if (rlang::is_bare_numeric(x$power, 1)) x$power else 1
      }
      else 1
    }
  )
}
