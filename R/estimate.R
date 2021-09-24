#' Estimate joint regression model for intervention costs and effects (QALYs)
#'
#' Estimate a multivariate generalised linear model for the joint incremental
#'     costs and QALYs from a randomised trial (or other comparable data
#'     source). Models can be estimated as either a Multivariate Covariance
#'     Generalized Linear Model (MCGLM; see
#'     \cite{Bonat & \enc{Jørgensen}{Jorgensen} 2016}), using the \code{mcglm}
#'     package (\cite{Bonat 2018}); or a Multivariate Generalized Linear Mixed
#'     Model (MGLMM) via Penalized Quasi-Likelihood
#'     (\cite{Achana et al. 2021}).
#'
#' The standard model specification provides `QALYs`, `costs`, `treatment`, and
#'     (optional) `covars` and `centre`/`cluster`, to estimate a bivariate
#'     regression with common RHS variables and optional clustering by trial
#'     centre or clustered randomisation. If a different specification is
#'     required (e.g., estimating additional outcomes beyond QALYs and Costs,
#'     or using different RHS variables in each equation), a custom model
#'     specification can be passed to \code{linear_pred} (for \code{mcglm}) or
#'     \code{mvfixed} (for \code{mglmmPQL}). See the \code{\link[mcglm]{mcglm}}
#'     documentation for details of the model specification in this case (the
#'     same format is used for both arguments).
#'
#' For either the standard or custom model specifications, arguments
#'     \code{matrix_pred}, \code{link}, and \code{variance} (for `mcglm`) or
#'     \code{family} and \code{random} (for `mglmmPQL`) can optionally be
#'     specified to override the corresponding default values. See the
#'     documentation for \code{\link[mcglm]{mcglm}} and \code{\link[nlme]{lme}}
#'     for the specification of these arguments.
#'
#' If not specified, \code{matrix_pred} defaults to an identity matrix (for
#'     each response variable) or identity matrix and cluster-specific terms if
#'     a cluster/centre variable is defined; \code{link} defaults to
#'     \code{"identity"} for QALYs and \code{"log"} for Costs if the default
#'     formula specification is used, or \code{"identity"} for all response
#'     variables if a custom \code{linear_pred} specification is provided; and
#'     \code{variance} defaults to \code{"constant"} for QALYs and
#'     \code{"tweedie"} for Costs (\code{"constant"} for all response variables
#'     if a custom \code{linear_pred} specification is provided).
#'
#' Defaults for the `mglmmPQL` method are \code{family} =
#'     \code{stats::gaussian} for QALYs and \code{stats::quasipoisson} for
#'     Costs; and \code{random} = \code{~outvar - 1 (| centre)} (random
#'     intercepts for each outcome, within centre/cluster if applicable).
#'
#' These defaults correspond to a linear regression model for QALYs and a
#'     Poisson-like GLM with log link for Costs, or a linear regression model
#'     for all responses for a custom \code{"linear_pred"}/\code{mvfixed}
#'     specification.
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
#' @param method \code{"mcglm"} or \code{"glmmPQL"}, specifying which model
#'     fitting algorithm to use.
#' @param linear_pred,mvfixed (optional) A list of formulae specifying the
#'     different model components to be estimated. If specified, over-rides the
#'     model specification in `QALYs`, `costs`, and optional `covars`. See
#'     \code{\link[mcglm]{mcglm}} for details (if provided, these arguments are
#'     passed to `mcglm`/`mglmmPQL` unchanged).
#' @param matrix_pred (optional) A list of matrices to be used on the matrix
#'     linear predictor. If not specified, the default is to use an identity
#'     matrix for both components, with random effects on any variables
#'     specified in `cluster`. If specified, over-rides the cluster
#'     specification in `cluster`. See \code{\link[mcglm]{mcglm}} for details
#'     (if provided, this argument is passed to `mcglm` unchanged).
#' @param link (optional) A list of link function names. See
#'     \code{\link[mcglm]{mcglm}} for details (if provided, this argument is
#'     passed to `mcglm` unchanged). If not specified, the default is to use an
#'     "identity" link for QALYs and a "log" link for Costs.
#' @param variance (optional) A list of link function names. See
#'     \code{\link[mcglm]{mcglm}} for details (if provided, this argument is
#'     passed to `mcglm` unchanged). If not specified, the default is to use a
#'     "constant" variance function for QALYs and a "tweedie" variation
#'     function for Costs.
#' @param family (optional) A list of family functions specifying the GLM error
#'     distribution and link function for each outcome, for
#'     \code{method = "mglmmPQL"}. Each element of the list should be a valid
#'     `family` value to be passed to \code{\link[stats]{glm}}. If not
#'     specified, the default is to use the `gaussian` family (i.e. OLS) for
#'     QALYs and `quasipoisson` (with a log link) for Costs.
#' @param correlation,weights Passed to \code{\link[nlme]{lme}}. If not
#'     specified, `weights` defaults to `nlme::varIdent(form = ~1 | outvar)`
#'     (allowing different variances for each outcome variable).
#' @param random (optional) Specification of the random effects component of
#'     the model for \code{method = "mglmmPQL"}. See \code{\link[nlme]{lme}}
#'     for details. If not specified, the default is
#'     \code{~outvar - 1 | centre} (or \code{~outvar - 1 | cluster}) if
#'     `centre` (`cluster`) is specified, \code{~outvar - 1} otherwise,
#'     corresponding to random intercepts for each outcome (within
#'     centre/cluster).
#' @param nlme_method \code{"REML"} (default) or \code{"ML"}; the `method`
#'     argument of \code{\link[nlme]{lme}}.
#' @param na.action (optional) What should happen when the data contain `NA`s.
#'     See \code{\link[nlme]{lme}}.
#' @param max_iter (optional) Maximum number of McGLM or PQL iterations. NOTE:
#'     Use \code{control = nlme::lmeControl(maxIter = <x>)} to set the maximum
#'     number of iterations for the `lme` optimization algorithm.
#' @param verbose Whether to print messages from the model fitting function
#'     (default=`FALSE`).
#' @param control A list of arguments to be passed to the fitting algorithm
#'     (argument `control_algorithm` of \code{\link[mcglm]{mcglm}} for method
#'     \code{"mcglm"}; `control` (returned via the function
#'     \code{\link[nlme]{lmeControl}}) of \code{\link[nlme]{lme}} for method
#'     \code{"mglmmPQL"}).
#' @param ... Further optional arguments to be passed to the underlying model
#'     fitting function.
#'
#' @return An object of class `cea_estimate` inheriting from `mcglm`.
#'
#' @export
estimate <- function(QALYs, costs, treatment, covars, data, centre = NULL, cluster = NULL,
                     method = "mcglm", linear_pred = NULL, matrix_pred = NULL, link = NULL,
                     variance = NULL, mvfixed = NULL, random = NULL, family = NULL,
                     correlation = NULL, weights = NULL, nlme_method = "REML",
                     na.action = "na.omit", max_iter = 50, verbose = FALSE, control = NULL,
                     ...) {
  UseMethod("estimate", data)
}

#' @export
estimate.data.frame <- function(QALYs, costs, treatment, covars, data, centre = NULL,
                                cluster = NULL, method = "mcglm", linear_pred = NULL,
                                matrix_pred = NULL, link = NULL, variance = NULL, mvfixed = NULL,
                                random = NULL, family = NULL, correlation = NULL, weights = NULL,
                                nlme_method = "REML", na.action = "na.omit", max_iter = 50,
                                verbose = FALSE, control = NULL, ...) {
  cl <- match.call()

  if (!rlang::is_string(method, c("mcglm", "mglmmPQL"))) stop_invalid_method(method)
  if (!rlang::is_string(treatment)) stop_not_string("treatment")
  if (!(treatment %in% names(data))) stop_variable_not_found(treatment, "data")
  if (!(is_valid_treatment(data[[treatment]])))
    stop_invalid_treatment(treatment, class(data[[treatment]]))
  if (!is.factor(data[[treatment]])) data[[treatment]] <- as.integer(data[[treatment]])

  if (method == "mcglm") {
    if (!rlang::is_installed("mcglm")) stop_pkg_not_installed("mcglm", "'mcglm' method")

    if (is.null(link)) {
      if (is.null(linear_pred)) link <- c("identity", "log")
      else link <- rep("identity", length(linear_pred))
    }
    if (is.null(variance)) {
      if (is.null(linear_pred)) variance <- c("constant", "tweedie")
      else variance <- rep("constant", length(linear_pred))
    }

    if (is.null(linear_pred)) {
      if (!rlang::is_string(QALYs)) stop_not_string("QALYs")
      if (!rlang::is_string(costs)) stop_not_string("costs")
      if (!(QALYs %in% names(data))) stop_variable_not_found(QALYs, "data")
      if (!(costs %in% names(data))) stop_variable_not_found(costs, "data")
      if (!missing(covars)) {
        if (!rlang::is_character(covars)) stop_not_character("covars")
        if (!all(covars %in% names(data)))
          stop_variable_not_found(covars[which.max(!(covars %in% names(data)))], "data")
      } else covars <- character()

      form_QALYs <- stats::reformulate(c(treatment, covars), QALYs)
      form_costs <- stats::reformulate(c(treatment, covars), costs)

      linear_pred <- list(QALYs = form_QALYs, Costs = form_costs)
    } else {
      if (!missing(QALYs) || !missing(costs) || !missing(covars))
        warn_formula_override("linear_pred")
    }

    if (is.null(matrix_pred)) {
      Z <- mcglm::mc_id(data)
      if (!is.null(cluster)) {
        if (!is.null(centre)) stop_cluster_centre()
        if (!rlang::is_string(cluster)) stop_not_string("cluster")
        if (!(cluster %in% names(data))) stop_variable_not_found(cluster, "data")
        if (!is.factor(data[[cluster]])) {
          warn_not_factor("cluster", cluster)
          data[[cluster]] <- as.factor(data[[cluster]])
        }
        form_cluster <- stats::reformulate(cluster, intercept = FALSE)
        Z <- c(Z, mcglm::mc_mixed(form_cluster, data))
      } else if (!is.null(centre)) {
        if (!rlang::is_string(centre)) stop_not_string("centre")
        if (!(centre %in% names(data))) stop_variable_not_found(centre, "data")
        if (!is.factor(data[[centre]])) {
          warn_not_factor("centre", centre)
          data[[centre]] <- as.factor(data[[centre]])
        }
        form_centre <- stats::reformulate(centre, intercept = FALSE)
        Z <- c(Z, mcglm::mc_mixed(form_centre, data))
      }
      matrix_pred <- rep(list(Z), length(linear_pred))
    } else {
      if (!is.null(cluster)) warn_cluster_override("matrix_pred", "cluster")
      if (!is.null(centre)) warn_cluster_override("matrix_pred", "centre")
    }

    if (is.null(control)) control <- list(max_iter = max_iter)

    out <- if (verbose) {
      mcglm::mcglm(linear_pred = linear_pred, matrix_pred = matrix_pred, link = link,
                   variance = variance, data = data, control_algorithm = control, ...)
    } else with_sink(
      tempfile(),
      mcglm::mcglm(linear_pred = linear_pred, matrix_pred = matrix_pred, link = link,
                   variance = variance, data = data, control_algorithm = control, ...)
    )
    class(out) <- c("cea_mcglm", "cea_estimate", class(out))
  } else if (method == "mglmmPQL") {
    if (!rlang::is_installed("nlme")) stop_pkg_not_installed("nlme", "'mglmmPQL' method")

    if (is.null(family)) {
      if (is.null(mvfixed)) family <- c("gaussian", "quasipoisson")
      else family <- rep("gaussian", length(mvfixed))
    }

    if (is.null(mvfixed)) {
      if (!rlang::is_string(QALYs)) stop_not_string("QALYs")
      if (!rlang::is_string(costs)) stop_not_string("costs")
      if (!(QALYs %in% names(data))) stop_variable_not_found(QALYs, "data")
      if (!(costs %in% names(data))) stop_variable_not_found(costs, "data")
      if (!missing(covars)) {
        if (!rlang::is_character(covars)) stop_not_character("covars")
        if (!all(covars %in% names(data)))
          stop_variable_not_found(covars[which.max(!(covars %in% names(data)))], "data")
      } else covars <- character()

      form_fixed <- stats::reformulate(c(treatment, covars), "value")

      mvfixed <- list(QALYs = form_fixed, Costs = form_fixed)
      outcomes <- c(QALYs, costs)
    } else {
      if (!missing(QALYs) || !missing(costs) || !missing(covars))
        warn_formula_override("mvfixed")
      outcomes <- sapply(mvfixed, function(fm) rlang::as_string(rlang::f_lhs(fm)))
      if (!all(outcomes %in% names(data)))
        stop_variable_not_found(outcomes[which.max(!(outcomes %in% names(data)))], "data")
      mvfixed <- lapply(mvfixed, function(fm) {rlang::f_lhs(fm) <- quote(value); fm})
      if (is.null(names(mvfixed))) names(mvfixed) <- outcomes
      else {
        nm_miss <- names(mvfixed) == ""
        names(mvfixed)[nm_miss] <- outcomes[nm_miss]
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
        random <- stats::as.formula(paste0("~ outvar - 1 | ", cluster))
      } else if (!is.null(centre)) {
        if (!rlang::is_string(centre)) stop_not_string("centre")
        if (!(centre %in% names(data))) stop_variable_not_found(centre, "data")
        if (!is.factor(data[[centre]])) {
          warn_not_factor("centre", centre)
          data[[centre]] <- as.factor(data[[centre]])
        }
        random <- stats::as.formula(paste0("~ outvar - 1 | ", centre))
      } else if (length(mvfixed) > 1) {
        random <- ~ outvar - 1 | .cons
        data$.cons <- 1
      } else {
        random <- ~ 1 | .cons
        data$.cons <- 1
      }
    } else {
      if (!is.null(cluster)) warn_cluster_override("random", "cluster")
      if (!is.null(centre)) warn_cluster_override("random", "centre")
    }

    if (is.null(control)) control <- nlme::lmeControl(maxIter = 100, opt = c("nlminb"))

    if (is.null(weights) && length(mvfixed) > 1) weights <- nlme::varIdent(form = ~1 | outvar)

    dat <- make_data_longform(data, outcomes)

    out <- if (is.null(random)) {
      eval(rlang::expr(mglmmPQL(
        mvfixed = !!mvfixed, family = !!family, correlation = !!correlation,
        weights = !!weights, data = make_data_longform(data, !!outcomes), outcomevar = "outvar",
        method = !!nlme_method, niter = !!max_iter, verbose = !!verbose, na.action = !!na.action,
        control = control
      )))
    } else {
      eval(rlang::expr(mglmmPQL(
        mvfixed = !!mvfixed, random = !!random, family = !!family, correlation = !!correlation,
        weights = !!weights, data = make_data_longform(data, !!outcomes), outcomevar = "outvar",
        method = !!nlme_method, niter = !!max_iter, verbose = !!verbose, na.action = !!na.action,
        control = control
      )))

    }
    class(out) <- c("cea_mglmmPQL", "cea_estimate", class(out))
    out$data.mglmmPQL <- out$data
    out$data <- data
    out$mvfixed <- mvfixed
  }

  attr(out, "call") <- cl
  attr(out, "tx") <- treatment
  attr(out, "centre") <- centre
  attr(out, "cluster") <- cluster
  out
}

#' @export
estimate.mids <- function(QALYs, costs, treatment, covars, data, centre = NULL, cluster = NULL,
                          method = "mcglm", linear_pred = NULL, matrix_pred = NULL, link = NULL,
                          variance = NULL, mvfixed = NULL, random = NULL, family = NULL,
                          correlation = NULL, weights = NULL, nlme_method = "REML",
                          na.action = "na.omit", max_iter = 50, verbose = FALSE, control = NULL,
                          ...) {
  if (!rlang::is_installed("mice") || utils::packageVersion("mice") < "3.0") {
    if (!rlang::is_installed("mice")) stop_pkg_not_installed("mice", "`estimate.mids()`", "3.0")
    stop_pkg_not_installed("mice", "`estimate.mids()`", "3.0", utils::packageVersion("mice"))
  }

  cl <- match.call()

  analyses <- as.list(seq_len(data$m))
  for (i in seq_along(analyses)) {
    data.i <- mice::complete(data, i)
    analyses[[i]] <- estimate(QALYs, costs, treatment, covars, data.i, centre = centre,
                              cluster = cluster, method = method, linear_pred = linear_pred,
                              matrix_pred = matrix_pred, link = link, variance = variance,
                              mvfixed = mvfixed, family = family, correlation = correlation,
                              weights = weights, random = random, nlme_method = nlme_method,
                              na.action = na.action, max_iter = max_iter, verbose = verbose,
                              control = control, ...)
  }
  object <- list(call = cl, call1 = data$call, nmis = data$nmis,
                 analyses = analyses)
  oldClass(object) <- c("cea_mira", "mira", "matrix")
  object
}

#' @export
estimate.default <- function(QALYs, costs, treatment, covars, data, centre = NULL, cluster = NULL,
                             method = "mcglm", linear_pred = NULL, matrix_pred = NULL, link = NULL,
                             variance = NULL, mvfixed = NULL, random = NULL, family = NULL,
                             correlation = NULL, weights = NULL, nlme_method = "REML",
                             na.action = "na.omit", max_iter = 50, verbose = FALSE, control = NULL,
                             ...) {
  cl <- match.call()

  if (!is.data.frame(data)) data <- as.data.frame(data)
  out <- estimate(QALYs, costs, treatment, covars, data, centre = centre, cluster = cluster,
                  method = method, linear_pred = linear_pred, matrix_pred = matrix_pred,
                  link = link, variance = variance, mvfixed = mvfixed, family = family,
                  correlation = correlation, weights = weights, random = random,
                  nlme_method = nlme_method, na.action = na.action, max_iter = max_iter,
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
    cat("    * Family:", getfamily(i, x$family)$family, "\n")
    cat("    * Link:", getfamily(i, x$family)$link, "\n")
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
