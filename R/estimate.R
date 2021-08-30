#' Estimate joint model for intervention costs and effects (QALYs)
#'
#' Estimate a multivariate covariance generalised linear model (\cite{Bonat &
#'     Jørgensen 2016}) for the joint incremental costs and QALYs from a
#'     randomised trial (or other comparable data source). Models are estimated
#'     using the \code{mcglm} package (\cite{Bonat 2018}).
#'
#' The standard model specification provides `QALYs`, `costs`, `treatment`, and
#'     (optional) `covars`, to estimate a bivariate regression with common RHS
#'     variables. If a different specification is required (e.g., estimating
#'     additional outcomes beyond QALYs and Costs, or using different RHS
#'     variables in each equation), a custom model specification can be passed
#'     to \code{linear_pred} (which will be passed directly to
#'     \code{\link[mcglm]{mcglm}}; see the \code{mcglm} documentation for
#'     details of the model specification in this case).
#'
#' In either specification, arguments \code{matrix_pred}, \code{link}, and
#'     \code{variance} can optionally be specified to override the
#'     corresponding default values.  See the documentation for
#'     \code{\link[mcglm]{mcglm}} for the specification of these arguments.
#'     If not specified, \code{matrix_pred} defaults to an identity matrix (for
#'     each response variable); \code{link} defaults to \code{"identity"} for
#'     QALYs and \code{"log"} for Costs if the default formula specification is
#'     used, or \code{"identity"} for all response variables if a custom
#'     \code{linear_pred} specification is provided; and \code{variance}
#'     defaults to \code{"constant"} for QALYs and \code{"tweedie"} for Costs
#'     (\code{"constant"} for all response variables if a custom
#'     \code{linear_pred} specification is provided). These defaults correspond
#'     to a linear regression model for QALYs and a Poisson-like GLM
#'     (equivalent to \code{family = "quasipoisson"} in base \code{glm}) for
#'     Costs, or a linear regression model for all responses for a custom
#'     \code{"linear_pred"} specification.
#'
#' @references
#' Bonat WH, Jørgensen B. \emph{Multivariate covariance generalized linear
#'     models}. J Royal Stat Soc 2016;65:649-75.
#'     \url{https://doi.org/10.1111/rssc.12145}
#'
#' Bonat WH. \emph{Multiple response variables regression models in R: The
#'     mcglm package}. J Stat Soft 2018;84(4):1-30.
#'     \url{https://doi.org/10.18637/jss.v084.i04}
#'
#' @param QALYs,costs,treatment Character strings naming the variables in
#'     `data` representing QALYs, costs, and treatment assignment,
#'     respectively.
#' @param covars (optional) Character vector naming variables in `data`
#'     included as (baseline) covariates in the regression models.
#' @param data A data frame (or object coercible by
#'     \code{\link[base]{as.data.frame}} to a data frame) or a `mids` object
#'     containing the variables in the model.
#' @param linear_pred (optional) A list of formula specifying the different
#'     model components to be estimated. If specified, over-rides the model
#'     specification in `QALYs`, `costs`, and optional `covars`. See
#'     \code{\link[mcglm]{mcglm}} for details (if provided, this argument is
#'     passed to `mcglm` unchanged).
#' @param matrix_pred (optional) A list of matrices to be used on the matrix
#'     linear predictor. See \code{\link[mcglm]{mcglm}} for details (if
#'     provided, this argument is passed to `mcglm` unchanged). If not
#'     specified, the default is to use an identity matrix for both components.
#' @param link (optional) A list of link function names. See
#'     \code{\link[mcglm]{mcglm}} for details (if provided, this argument is
#'     passed to `mcglm` unchanged). If not specified, the default is to use an
#'     "identity" link for QALYs and a "log" link for Costs.
#' @param variance (optional) A list of link function names. See
#'     \code{\link[mcglm]{mcglm}} for details (if provided, this argument is
#'     passed to `mcglm` unchanged). If not specified, the default is to use a
#'     "constant" variance function for QALYs and a "tweedie" variation
#'     function for Costs.
#' @param ... Optional arguments to be passed to \code{\link[mcglm]{mcglm}}.
#'
#' @return An object of class `cea_estimate` inheriting from `mcglm`.
#'
#' @export
estimate <- function(QALYs, costs, treatment, covars, data,
                     linear_pred = NULL, matrix_pred = NULL, link = NULL, variance = NULL, ...) {
  UseMethod("estimate", data)
}

#' @export
estimate.data.frame <- function(QALYs, costs, treatment, covars, data, linear_pred = NULL,
                                matrix_pred = NULL, link = NULL, variance = NULL, ...) {
  cl <- match.call()

  if (is.null(link)) {
    if (is.null(linear_pred)) link <- c("identity", "log")
    else link <- rep("identity", length(linear_pred))
  }
  if (is.null(variance)) {
    if (is.null(linear_pred)) variance <- c("constant", "tweedie")
    else variance <- rep("constant", length(linear_pred))
  }

  if (!rlang::is_string(treatment)) stop_not_string("treatment")
  if (!(treatment %in% names(data))) stop_variable_not_found(treatment, "data")
  if (!(is_valid_treatment(data[[treatment]])))
    stop_invalid_treatment(treatment, class(data[[treatment]]))
  if (!is.factor(data[[treatment]])) data[[treatment]] <- as.integer(data[[treatment]])

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
      warn_formula_override()
  }
  n_outcome <- length(linear_pred)

  if (is.null(matrix_pred)) {
    matrix_pred <- rep(list(mcglm::mc_id(data)), n_outcome)
  }

  out <- with_sink(
    tempfile(),
    mcglm::mcglm(linear_pred = linear_pred, matrix_pred = matrix_pred, link = link,
                 variance = variance, data = data, ...)
  )

  class(out) <- c("cea_estimate", class(out))
  attr(out, "call") <- cl
  attr(out, "tx") <- treatment
  out
}

#' @export
estimate.mids <- function(QALYs, costs, treatment, covars, data, linear_pred = NULL,
                          matrix_pred = NULL, link = NULL, variance = NULL, ...) {
  if (!rlang::is_installed("mice", version = "3.0")) {
    if (!rlang::is_installed("mice")) stop_mice_not_installed()
    stop_mice_not_installed(utils::packageVersion("mice"))
  }

  cl <- match.call()

  analyses <- as.list(seq_len(data$m))
  for (i in seq_along(analyses)) {
    data.i <- mice::complete(data, i)
    analyses[[i]] <- estimate(QALYs, costs, treatment, covars, data.i, linear_pred, matrix_pred,
                              link, variance, ...)
  }
  object <- list(call = cl, call1 = data$call, nmis = data$nmis,
                 analyses = analyses)
  oldClass(object) <- c("cea_mira", "mira", "matrix")
  object
}

#' @export
estimate.default <- function(QALYs, costs, treatment, covars, data, linear_pred = NULL,
                             matrix_pred = NULL, link = NULL, variance = NULL, ...) {
  cl <- match.call()

  if (!is.data.frame(data)) data <- as.data.frame(data)
  out <- estimate(QALYs, costs, treatment, covars, data, linear_pred = NULL,
                  matrix_pred = NULL, link = NULL, variance = NULL, ...)
  attr(out, "call") <- cl
  out
}

#' @export
print.cea_estimate <- function(x, ...) {
  cat("===============================================\n")
  cat("=== Cost-Effectiveness Regression Estimates ===\n")
  cat("===============================================\n\n")

  cat("Call:\n", rlang::quo_text(attr(x, "call")), "\n\n")

  cat("------------------\n")
  cat("Univariate Models:\n\n")
  for (i in seq_along(x$linear_pred)) {
    cat("  ", names(x$linear_pred)[[i]], ": ", rlang::as_label(x$linear_pred[[i]]), "\n", sep = "")
    cat("    * Link function:", x$link[[i]], "\n")
    cat("    * Variance function:", x$variance[[i]], "\n")
    cat("    * Covariance function:", x$covariance[[i]], "\n\n")
  }

  cat("------------------\n")
  cat("Incremental Treatment Effects:\n")
  if (is_factor_tx(x)) {
    cat("        ", pad(extract_tx(x), 10), "\n")
  }
  cat("  QALYs:", sprintf("%+10.3f", QALYs(x)), "\n")
  cat("  Costs:", sprintf("%+10.0f", Costs(x)), "\n")
  cat("  ICER: ", sprintf("%10.0f", ICER(x)), "\n\n")

  cat("===============================================\n")

  return(invisible(x))
}

#' @export
print.cea_mira <- function(x, ...) {
  m = length(x$analyses)

  cat("================================================================\n")
  cat("=== Multiply-Imputed Cost-Effectiveness Regression Estimates ===\n")
  cat("================================================================\n")

  cat("Based on", m, "imputed datasets.\n\n")

  cat("Call:\n", rlang::quo_text(x$call), "\n\n")

  cat("Data:\n", rlang::quo_text(x$call1), "\n\n")

  cat("------------------\n")
  cat("Univariate Models:\n\n")
  for (i in seq_along(x$analyses[[1]]$linear_pred)) {
    cat("  ", names(x$analyses[[1]]$linear_pred)[[i]], ": ",
        rlang::as_label(x$analyses[[1]]$linear_pred[[i]]), "\n", sep = "")
    cat("    * Link function:", x$analyses[[1]]$link[[i]], "\n")
    cat("    * Variance function:", x$analyses[[1]]$variance[[i]], "\n")
    cat("    * Covariance function:", x$analyses[[1]]$covariance[[i]], "\n\n")
  }

  cat("------------------\n")
  cat("Incremental Treatment Effects:\n")
  cat("(From first imputed dataset; use `pool_cea()` to compute pooled estimates)\n")
  if (is_factor_tx(x$analyses[[1]])) {
    cat("        ", pad(extract_tx(x$analyses[[1]]), 10), "\n")
  }
  cat("  QALYs:", sprintf("%+10.3f", QALYs(x$analyses[[1]])), "\n")
  cat("  Costs:", sprintf("%+10.0f", Costs(x$analyses[[1]])), "\n")
  cat("  ICER: ", sprintf("%10.0f", ICER(x$analyses[[1]])), "\n\n")

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
