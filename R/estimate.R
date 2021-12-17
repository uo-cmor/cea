#' Estimate joint regression model for intervention costs and effects (QALYs)
#'
#' Estimate a multivariate generalised linear model for the joint incremental
#'     costs and QALYs from a randomised trial (or other comparable data
#'     source). Models can be estimated as a Multivariate Covariance
#'     Generalized Linear Model (MCGLM; see
#'     \cite{Bonat & J\/orgensen 2016}), using the \code{mcglm}
#'     package (\cite{Bonat 2018}); a Multivariate Generalized Linear Mixed
#'     Model (MGLMM) via Penalized Quasi-Likelihood
#'     (\cite{Achana et al. 2021}); or a Bayesian generalized multivariate
#'     model, using the \code{brms} package (\cite{B\/"urkner 2017, 2018}).
#'
#' The standard model specification provides `QALYs`, `costs`, `treatment`, and
#'     optionally `covars` and `centre`/`cluster`, to estimate a bivariate
#'     regression with common RHS variables and optional clustering by trial
#'     centre or clustered randomisation.
#'
#' If a different specification is required (e.g., estimating additional
#'     outcomes beyond QALYs and Costs, or using different RHS variables in
#'     each equation), a custom model specification can be passed to
#'     \code{fixed} as a list of formulae. In this case, `family` should also
#'     be used to specify the GLM 'family' for each outcome.
#'
#' Note the following details for model specification for each method:
#'
#' \strong{mcglm:}
#'
#' \itemize{
#'     \item Model specification arguments `linear_pred`, `matrix_pred`,
#'     `link`, and `variance` are constructed via the `estimate()` arguments
#'     `QALYs`, `costs`, `treatment`, and `covars`; `centre` or
#'     `cluster`; and `family` (for which see below), and should not be
#'     passed directly. For custom model specifications, use the arguments
#'     `fixed`, `random`, and `family`.
#'     \item Instead of a standard GLM family specification, `family` can also
#'     be provided as a list where each element (corresponding to each model
#'     outcome) is a named list with elements "family" (a name for the custom
#'     family), "link", "variance" (passed as the corresponding arguments to
#'     \code{\link[mcglm]{mcglm}}), and "power" (the (starting) power
#'     parameter, currently only used for `variance = "tweedie"`). For the
#'     "tweedie" variance function, a power value 0 corresponds to a gaussian
#'     distribution, 1 to the (quasi-)Poisson distribution, 2 to the gamma
#'     distribution, and 3 to the inverse gaussian. (As in `mcglm`, the dafault
#'     is to fix the power parameter to its initial value.)
#'     \item The `control_initial` argument is derived by fitting separate GLM
#'     models to each outcome variable (as in the `mcglm` default) and cannot
#'     be directly set by the user. The initial power parameter can be set via
#'     `family` as descried above.
#' }
#'
#' \strong{mglmmPQL:}
#'
#' \itemize{
#'     \item The `lme` argument `method` is renamed `nlme_method` to avoid a
#'     clash with the same-named argument to `estimate`.
#'     \item Formulae specified in `random` or `weights` (see below) can refer
#'     to the reserved name `outvar`, an indicator variable identifying the
#'     outcome variable to which an observation belongs in the transformed
#'     long-form (stacked) data used in the underlying `lme` estimation.
#'     \item By default, `weights` is set to
#'     `nlme::varIdent(form = ~1 | outvar)`, to allow different variances for
#'     each outcome variable. See \code{\link[nlme]{varIdent}} for details.
#'     \item `max_iter` can be used to specify the maximum number of PQL
#'     iterations. Use
#'     \code{control =\link[nlme:lmeControl]{nlme::lmeControl}(maxIter = <x>)}
#'     to set the maximum number of iterations for the `lme` optimization
#'     algorithm.
#' }
#'
#' @references
#' Achana F, Gallacher D, Oppong R, et al. \emph{Multivariate generalized
#'     linear mixed-effects models for the analysis of clinical trial-based
#'     cost-effectiveness data}. Med Decis Making 2021;41(6):667-84.
#'     \url{https://doi.org/10.1177/0272989X211003880}
#'
#' Bonat WH. \emph{Multiple response variables regression models in R: The
#'     mcglm package}. J Stat Soft 2018;84(4):1-30.
#'     \url{https://doi.org/10.18637/jss.v084.i04}
#'
#' Bonat WH, \enc{Jørgensen}{Jorgensen} B. \emph{Multivariate covariance
#'     generalized linear models}. J Royal Stat Soc 2016;65:649-75.
#'     \url{https://doi.org/10.1111/rssc.12145}
#'
#' \enc{Bürkner}{Burkner} P-C. \emph{brms: An R package for Bayesian multilevel
#'     models using Stan}. J Stat Soft 2017;80(1):1-28.
#'     \url{https://doi.org/10.18637/jss.v080.i01}
#'
#' \enc{Bürkner}{Burkner} P-C. \emph{Advanced Bayesian multilevel modeling with
#'     the R package brms}. R Journal 2018;10(1):395-411.
#'     \url{https://doi.org/10.32614/RJ-2018-017}
#'
#' @param QALYs,costs,treatment Character strings naming the variables in
#'     `data` representing QALYs, costs, and treatment assignment,
#'     respectively.
#' @param covars (optional) Character vector naming variables in `data`
#'     included as (baseline) covariates in the regression models.
#' @param data A data frame (or object coercible by
#'     \code{\link[base]{as.data.frame}} to a data frame) or a `mids` object
#'     containing the variables in the model.
#' @param centre (optional) Character string naming a variable in `data`
#'     signifying centre membership in a multicentre trial.
#' @param cluster (optional) Character vector naming a variable in `data`
#'     signifying cluster membership in a cluster-randomised trial. NOTE: At
#'     present, `centre` and `cluster` do exactly the same thing (estimate a
#'     mixed-effects model with centre/cluster random effects), and only one of
#'     these arguments should be specified.
#' @param family (optional) A list of family functions specifying the
#'     distribution and link function for each outcome variable. Each element
#'     of the list should be a valid `family` value specified as for
#'     \code{\link[stats]{glm}}. If not specified, the default is to use
#'     \code{\link[stats]{gaussian}()} (i.e. OLS) for QALYs and
#'     \code{\link[stats]{Gamma}("log")} for Costs.
#' @param prior For `method = 'brms'` only, specification of priors for
#'     Bayesian model. See \code{\link[brms]{brm}} and
#'     \code{\link[brms]{set_prior}} for details. If not specified, uses
#'     `"normal(0, 5)"` for fixed effects coefficients and `brm` defaults for
#'     all other parameters.
#' @param method \code{"mcglm"}, \code{"mglmmPQL"}, or \code{"brms"},
#'     specifying which modelling approach to use.
#' @param fixed An alternative way to specify the 'fixed effects' component of
#'     the model. Required if the RHS variables should differ for each equation
#'     or outcomes other than QALYs and Costs are to be estimated. If used,
#'     should be a (optionally named) list of two-sided model formulae
#'     specifying the equations for each outcome measure, and will override any
#'     values specified in `QALYs`, `costs`, and `covars`; the variable
#'     specified in `treatment` must be included in the RHS of each formula. If
#'     `fixed` does not have names, the LHS of each formula will be used as
#'     component names.
#' @param random (optional) Specification of the random effects component of
#'     the model. Corresponds to the `matrix_pred` argument of
#'     \code{\link[mcglm]{mcglm}} and the `random` argument of
#'     \code{\link[nlme]{lme}}; see the corresponding package help for details.
#'     If not specified, the default is to include centre/cluster random
#'     effects (intercepts) for each outcome (if applicable).
#' @param verbose Whether to print messages from the model fitting function
#'     (default=`FALSE`).
#' @param control A list of arguments to be passed to the fitting algorithm
#'     (argument `control_algorithm` of \code{\link[mcglm]{mcglm}} for method
#'     \code{"mcglm"}; `control` (returned via the function
#'     \code{\link[nlme]{lmeControl}}) of \code{\link[nlme]{lme}} for method
#'     \code{"mglmmPQL"}).
#' @param ... Further optional arguments to be passed to the underlying model
#'     fitting function specified by `method`
#'     (\code{\link[mcglm]{mcglm}}/\code{\link[nlme]{lme}}). See also the
#'     'Details' section for additional considerations for specifying these
#'     arguments.
#'
#' @return An object of class `cea_estimate` inheriting from `mcglm`.
#'
#' @export
estimate <- function(QALYs, costs, treatment, covars, data, centre = NULL, cluster = NULL,
                     family = NULL, prior = NULL, method = "mcglm", fixed = NULL, random = NULL,
                     verbose = FALSE, control = NULL, ...) {
  UseMethod("estimate", data)
}

#' @export
estimate.data.frame <- function(QALYs, costs, treatment, covars, data, centre = NULL,
                                cluster = NULL, family = NULL, prior = NULL, method = "mcglm",
                                fixed = NULL, random = NULL, verbose = FALSE, control = NULL,
                                ...) {
  cl <- match.call()

  switch(
    method,
    mcglm = if (!rlang::is_installed("mcglm")) stop_pkg_not_installed("mcglm", "'mcglm' method"),
    mglmmPQL = if (!rlang::is_installed("nlme")) stop_pkg_not_installed("nlme",
                                                                        "'mglmmPQL' method"),
    brms = if (!rlang::is_installed("brms")) stop_pkg_not_installed("brms", "'brms' method"),
    stop_invalid_method(method)
  )

  if (!rlang::is_string(treatment)) stop_not_string("treatment")
  if (!(treatment %in% names(data))) stop_variable_not_found(treatment, "data")
  if (!(is_valid_treatment(data[[treatment]])))
    stop_invalid_treatment(treatment, class(data[[treatment]]))
  if (!is.factor(data[[treatment]])) data[[treatment]] <- as.integer(data[[treatment]])

  if (is.null(family)) {
    if (is.null(fixed)) family <- list(stats::gaussian(), stats::Gamma("log"))
    else family <- rep(list(stats::gaussian()), length(fixed))
  } else {
    if (!rlang::is_bare_vector(family)) stop_wrong_type("family",
                                                        "a list/vector of family functions")
  }
  family <- lapply(seq_along(family), function(i) get_family(i, family))

  if (is.null(fixed)) {
    if (!rlang::is_string(QALYs)) stop_not_string("QALYs")
    if (!rlang::is_string(costs)) stop_not_string("costs")
    if (!(QALYs %in% names(data))) stop_variable_not_found(QALYs, "data")
    if (!(costs %in% names(data))) stop_variable_not_found(costs, "data")

    if (!missing(covars)) {
      if (!rlang::is_character(covars)) stop_not_character("covars")
      if (!all(covars %in% names(data)))
        stop_variable_not_found(covars[which.max(!(covars %in% names(data)))], "data")
    } else covars <- character()

    fixed <- list(QALYs = stats::reformulate(c(treatment, covars), QALYs),
                  Costs = stats::reformulate(c(treatment, covars), costs))
    outcomes <- c(QALYs, costs)
  } else {
    if (!missing(QALYs) || !missing(costs) || !missing(covars))
      warn_formula_override("fixed")
    outcomes <- sapply(fixed, function(fm) rlang::as_string(rlang::f_lhs(fm)))
    if (!all(outcomes %in% names(data)))
      stop_variable_not_found(outcomes[which.max(!(outcomes %in% names(data)))], "data")
    if (is.null(names(fixed))) {
      names(fixed) <- outcomes
    } else {
      nm_miss <- names(fixed) == "" | is.na(names(fixed))
      names(fixed)[nm_miss] <- outcomes[nm_miss]
    }
  }

  if (is.null(random)) {
    if (!is.null(cluster)) {
      if (!is.null(centre)) stop_cluster_centre()
      if (!rlang::is_string(cluster)) stop_not_string("cluster")
      if (!(cluster %in% names(data))) stop_variable_not_found(cluster, "data")
      if (!is.factor(data[[cluster]])) {
        warn_not_factor("cluster", cluster)
        data[[cluster]] <- as.factor(data[[cluster]])
      }
      random <- make_random_spec(data, method, length(fixed), cluster)
    } else if (!is.null(centre)) {
      if (!rlang::is_string(centre)) stop_not_string("centre")
      if (!(centre %in% names(data))) stop_variable_not_found(centre, "data")
      if (!is.factor(data[[centre]])) {
        warn_not_factor("centre", centre)
        data[[centre]] <- as.factor(data[[centre]])
      }
      random <- make_random_spec(data, method, length(fixed), centre)
    } else {
      if (method == "mglmmPQL") data$.cons <- 1
      random <- make_random_spec(data, method, length(fixed))
    }
  } else {
    if (!is.null(cluster)) warn_cluster_override("random", "cluster")
    if (!is.null(centre)) warn_cluster_override("random", "centre")
  }


  out <- switch(
    method,
    mcglm = with_sink(
      if (verbose) NULL else tempfile(),
      {
        if (!("max_iter" %in% names(control))) control <- append(control, list(max_iter = 100))
        control_initial <- mcglm::mc_initial_values(
          linear_pred = fixed, matrix_pred = random, link = get_link(family),
          variance = extract_variance(family), covariance = rep("identity", length(fixed)),
          offset = rep(0, length(fixed)), data = data)
        control_initial$power <- extract_power(family)

        eval(rlang::expr(mcglm::mcglm(
          linear_pred = fixed, matrix_pred = random, link = get_link(family),
          variance = extract_variance(family), data = data, control_algorithm = control,
          control_initial = control_initial, ...
        )))
      }
    ),
    mglmmPQL = eval(rlang::expr(mglmmPQL(
      mvfixed = !!fixed, random = !!random, family = !!family,
      data = make_data_longform(data, !!outcomes), outcomevar = "outvar", verbose = verbose,
      control = control, ...
    ))),
    brms = {
      if (!is.null(random)) fixed <- lapply(fixed, function(f) stats::update(f, random))
      formula <- brms::mvbrmsformula(flist = fixed, rescor = FALSE)
      if (is.null(prior)) prior <- rlang::exec(
        rbind,
        !!!lapply(names(formula$forms),
                  function(resp) brms::set_prior("normal(0, 5)", class = "b", resp = resp))
      )
      iter <- if ("iter" %in% ...names()) ...elt(which(...names() == "iter")) else 2000
      brms::brm(formula, data, family, prior, refresh = if (isTRUE(verbose)) max(iter/10, 1) else 0,
                silent = 2 - verbose, control = control, ...)
    }
  )

  class(out) <- c(paste0("cea_", method), "cea_estimate", class(out))
  if (method == "mglmmPQL") {
    out$data.mglmmPQL <- out$data
    out$data <- data
    out$mvfixed <- fixed
  } else if (method == "brms") {
    names(out$family) <- names(fixed)
  }
  attr(out, "call") <- cl
  attr(out, "tx") <- treatment
  attr(out, "centre") <- centre
  attr(out, "cluster") <- cluster

  out
}

#' @export
estimate.mids <- function(QALYs, costs, treatment, covars, data, centre = NULL, cluster = NULL,
                          family = NULL, prior = NULL, method = "mcglm", fixed = NULL,
                          random = NULL, verbose = FALSE, control = NULL, ...) {
  if (!rlang::is_installed("mice") || utils::packageVersion("mice") < "3.0") {
    if (!rlang::is_installed("mice")) stop_pkg_not_installed("mice", "`estimate.mids()`", "3.0")
    stop_pkg_not_installed("mice", "`estimate.mids()`", "3.0", utils::packageVersion("mice"))
  }

  cl <- match.call()

  analyses <- as.list(seq_len(data$m))
  for (i in seq_along(analyses)) {
    data.i <- mice::complete(data, i)
    analyses[[i]] <- estimate(QALYs, costs, treatment, covars, data.i, centre = centre,
                              cluster = cluster, family = family, prior = prior, method = method,
                              fixed = fixed, random = random, verbose = verbose, control = control,
                              ...)
  }
  object <- list(call = cl, call1 = data$call, nmis = data$nmis,
                 analyses = analyses)
  oldClass(object) <- c("cea_mira", "mira", "matrix")
  object
}

#' @export
estimate.default <- function(QALYs, costs, treatment, covars, data, centre = NULL, cluster = NULL,
                             family = NULL, prior = NULL, method = "mcglm", fixed = NULL,
                             random = NULL, verbose = FALSE, control = NULL, ...) {
  cl <- match.call()

  if (!is.data.frame(data)) data <- as.data.frame(data)
  out <- estimate(QALYs, costs, treatment, covars, data, centre = centre, cluster = cluster,
                  family = family, prior = prior, method = method, fixed = fixed, random = random,
                  verbose = verbose, control = control, ...)
  attr(out, "call") <- cl
  out
}

#' @export
print.cea_mcglm <- function(x, ...) {
  cat("===============================================\n")
  cat("=== Cost-Effectiveness Regression Estimates ===\n")
  cat("===============================================\n\n")

  cat("Multivariate Covariate Generalized Linear Model\n\n")

  cat("Call:\n", rlang::quo_text(attr(x, "call")), "\n\n")

  cat("------------------\n")
  cat("Univariate Models:\n\n")
  for (i in seq_along(x$linear_pred)) {
    # define a `extract_models()` or similar to use here
    # -- then the same print function can be used for all `cea_estimate` subclasses
    nm <- names(x$linear_pred)[[i]]
    len_nm <- nchar(nm, type = "width")
    form <- deparse(x$linear_pred[[i]], width.cutoff = 80 - len_nm - 6)

    cat("  ", nm, ": ",
        paste(form, collapse = paste0("\n", strrep(" ", len_nm + 2))), "\n", sep = "")
    cat("    * Link function:", x$link[[i]], "\n")
    cat("    * Variance function:", x$variance[[i]], "\n")
    cat("    * Covariance function:", x$covariance[[i]], "\n\n")
  }

  cat("------------------\n")
  cat("Incremental Treatment Effects:\n")
  if (is_factor_tx(x)) {
    cat("        ", pad(extract_tx(x), 10), "\n")
  }
  if ("QALYs" %in% extract_outcomes(x)) cat("  QALYs:", sprintf("%+10.3f", QALYs(x)), "\n")
  if ("Costs" %in% extract_outcomes(x)) cat("  Costs:", sprintf("%+10.0f", Costs(x)), "\n")
  if (all(c("QALYs", "Costs") %in% extract_outcomes(x)))
    cat("  ICER: ", sprintf("%10.0f", ICER(x)), "\n\n")

  cat("===============================================\n")

  return(invisible(x))
}

#' @export
print.cea_mglmmPQL <- function(x, ...) {
  cat("===============================================\n")
  cat("=== Cost-Effectiveness Regression Estimates ===\n")
  cat("===============================================\n\n")

  cat("Multivariate Generalized Linear Mixed-Effects Model\n\n")

  cat("Call:\n", rlang::quo_text(attr(x, "call")), "\n\n")

  cat("------------------\n")
  cat("Univariate Models:\n\n")
  for (i in seq_along(x$mvfixed)) {
    # define a `extract_models()` or similar to use here
    # -- then the same print function can be used for all `cea_estimate` subclasses
    nm <- names(x$mvfixed)[[i]]
    len_nm <- nchar(nm, type = "width")
    x$mvfixed[[i]][[2]] <- rlang::sym(levels(x$data.mglmmPQL$outvar)[[i]])
    form <- deparse(x$mvfixed[[i]], width.cutoff = 80 - len_nm - 6)

    cat("  ", nm, ": ",
        paste(form, collapse = paste0("\n", strrep(" ", len_nm + 2))), "\n", sep = "")
    cat("    * Family:", get_family(i, x$family)$family, "\n")
    cat("    * Link:", get_family(i, x$family)$link, "\n")
  }

  cat("------------------\n")
  cat("Incremental Treatment Effects:\n")
  if (is_factor_tx(x)) {
    cat("        ", pad(extract_tx(x), 10), "\n")
  }
  if ("QALYs" %in% extract_outcomes(x)) cat("  QALYs:", sprintf("%+10.3f", QALYs(x)), "\n")
  if ("Costs" %in% extract_outcomes(x)) cat("  Costs:", sprintf("%+10.0f", Costs(x)), "\n")
  if (all(c("QALYs", "Costs") %in% extract_outcomes(x)))
    cat("  ICER: ", sprintf("%10.0f", ICER(x)), "\n\n")

  cat("===============================================\n")

  return(invisible(x))
}

#' @export
print.cea_brms <- function(x, ...) {
  cat("===============================================\n")
  cat("=== Cost-Effectiveness Regression Estimates ===\n")
  cat("===============================================\n\n")

  cat("Bayesian Generalized Multivariate Linear Model\n\n")

  cat("Call:\n", rlang::quo_text(attr(x, "call")), "\n\n")

  cat("------------------\n")
  cat("Univariate Models:\n\n")
  for (i in seq_along(x$formula$forms)) {
    nm <- names(x$formula$forms)[[i]]
    len_nm <- nchar(nm, type = "width")
    form <- deparse(x$formula$forms[[i]]$formula, width.cutoff = 80 - len_nm - 6)

    cat("  ", nm, ": ",
        paste(form, collapse = paste0("\n", strrep(" ", len_nm + 2))), "\n", sep = "")
    cat("    * Family:", get_family(i, x$family)$family, "\n")
    cat("    * Link:", get_family(i, x$family)$link, "\n")
  }

  cat("------------------\n")
  cat("Incremental Treatment Effects:\n")
  if (is_factor_tx(x)) {
    cat("        ", pad(extract_tx(x), 10), "\n")
  }
  if ("QALYs" %in% extract_outcomes(x)) cat("  QALYs:", sprintf("%+10.3f", QALYs(x)), "\n")
  if ("Costs" %in% extract_outcomes(x)) cat("  Costs:", sprintf("%+10.0f", Costs(x)), "\n")
  if (all(c("QALYs", "Costs") %in% extract_outcomes(x)))
    cat("  ICER: ", sprintf("%10.0f", ICER(x)), "\n\n")

  cat("===============================================\n")

  return(invisible(x))
}

#' @export
print.cea_mira <- function(x, ...) {
  m = length(x$analyses)
  x1 <- x$analyses[[1]]

  cat("================================================================\n")
  cat("=== Multiply-Imputed Cost-Effectiveness Regression Estimates ===\n")
  cat("================================================================\n")

  cat("Based on", m, "imputed datasets.\n\n")

  cat("Call:\n", rlang::quo_text(x$call), "\n\n")

  cat("Data:\n", rlang::quo_text(x$call1), "\n\n")

  cat("------------------\n")
  cat("Univariate Models:\n\n")
  for (i in seq_along(x1$linear_pred)) {
    nm <- names(x1$linear_pred)[[i]]
    len_nm <- nchar(nm, type = "width")
    form <- deparse(x1$linear_pred[[i]], width.cutoff = 80 - len_nm - 6)

    cat("  ", nm, ": ",
        paste(form, collapse = paste0("\n", strrep(" ", len_nm + 2))), "\n", sep = "")
    cat("    * Link function:", x1$link[[i]], "\n")
    cat("    * Variance function:", x1$variance[[i]], "\n")
    cat("    * Covariance function:", x1$covariance[[i]], "\n\n")
  }

  cat("------------------\n")
  cat("Incremental Treatment Effects:\n")
  cat("(From first imputed dataset; use `pool_cea()` to compute pooled estimates)\n")
  if (is_factor_tx(x1)) cat("        ", pad(extract_tx(x1), 10), "\n")
  cat("  QALYs:", sprintf("%+10.3f", QALYs(x1)), "\n")
  cat("  Costs:", sprintf("%+10.0f", Costs(x1)), "\n")
  cat("  ICER: ", sprintf("%10.0f", ICER(x1)), "\n\n")

  cat("===============================================\n")

  return(invisible(x))
}

is_factor_tx <- function(x) UseMethod("is_factor_tx")
is_factor_tx.cea_estimate <- function(x) is.factor(x$data[[attr(x, "tx")]])
is_factor_tx.cea_boot <- function(x) !is.null(nrow(x$t0))

pad <- function(x, width, cont = "~") {
  cont <- ifelse(nchar(x, "width") >= width, cont, "")
  x <- strtrim(x, width - 1 - nchar(cont))
  paste0(strrep(" ", width - nchar(x, "width") - nchar(cont, "width")), x, cont)
}
