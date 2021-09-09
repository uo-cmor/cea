make_data_longform <- function(data, outcomes) {
  out <- lapply(
    outcomes, dat = data,
    function(x, dat) {
      dat$value <- dat[[x]]
      dat$outvar <- x
      dat[, -which(names(dat) %in% outcomes)]
    }
  )
  out <- rlang::exec(rbind, !!!out)
  out$outvar <- factor(out$outvar, levels = outcomes)
  out
}

### Taken from Achana et al. MDM 2021;41(6):667-84. doi:10.1177/0272989X211003880

# Merge fixed-effects terms into one-multivariate fixed-effects formula
mergeFormula <- function(feFormulae) {
  lhs <- rhs <- list()
  nout <- length(feFormulae)
  if (nout == 1) return(feFormulae[[1]])
  for (j in 1:nout) {
    lhs[[j]] <- feFormulae[[j]][[2L]]
    rhs[[j]] <- strsplit(deparse(feFormulae[[j]][[3L]],width.cutoff = 500L), " \\+ ")[[1]]
    rhs[[j]][[length(rhs[[j]])]] <- strsplit(rhs[[j]][[length(rhs[[j]])]], "- 1")
  }
  unique(unlist(lhs, recursive = TRUE, use.names = TRUE))
  if (length(unique(lhs)) != 1) stop('both formulas must have the same response')
  rhs <- c(paste0(unlist(rhs, recursive = TRUE, use.names = TRUE), ":outvar"), "outvar")

  lhs <- unlist(lhs[[1]], recursive = TRUE, use.names = TRUE)  # create the merged rhs and lhs in character string form
  rhs[[length(rhs)]] <- paste(rhs[[length(rhs)]], "-1", sep = "")
  out <- stats::reformulate(rhs, lhs)
  environment(out) <- parent.frame()

  out
}

# Extract glm family for each response
getfamily <- function(j, family) {
  fm <- family[[j]]
  if (is.character(fm)) fm <- get(fm)
  if (is.function(fm)) fm <- fm()
  if (is.null(fm)) {
    print(fm)
    stop("'family' not recognized")
  }

  fm
}

# Fit univariate GLMs to each outcome and add linear predictor (eta),
# residuals (res), prior weights (w) and working weights (wz) to data frame
fitglm = function(fixed, family, data, weights) {
  fit0  = stats::glm(fixed, family, data, na.action = "na.omit")

  data.frame(fit0$data, y0 = fit0$y, eta = fit0$linear.predictors, res = fit0$residuals,
             w = fit0$prior.weights, wz = fit0$weights)
}

# Calculate mu = g-1(eta) and derivative g'(mu) for the linearization step
extractlinkinv <- function(j, data, family) {
  eta1 <- data[data$outvar == levels(as.factor(data$outvar))[j], "eta"]
  w1 <- data[data$outvar == levels(as.factor(data$outvar))[j], "w"]
  fm <- getfamily(j, family)
  mu1 <- fm$linkinv(eta1)
  mueta <- fm$mu.eta(eta1)
  wz1 <- w1*mueta^2 / fm$variance(mu1)

  data.frame(mu = mu1, mu.eta.val = mueta, wz = wz1)
}

# Export this one (main function to estimate the mglmm model via PQL):
mglmmPQL = function(mvfixed, random, family, correlation, weights, data, outcomevar,
                    method = "REML", niter = 200, verbose = TRUE, na.action = "na.omit",
                    control = nlme::lmeControl(maxIter = 100, opt = c("nlminb")), ...) {
  # `data` should be a stacked data frame (one row per outcome per participant), with a
  # factor/character variable indicating which outcome each row refers to

  # `mvfixed` is a list of formulae for the fixed effects part (must have the same LHS)

  # `random`, `correlation`, `weights`, `method`, `na.action`, `control` passed to `nlme::lme`


  # Set the name of the outcome (indicator) variable in `data` to "outvar"
  names(data)[names(data) == outcomevar] <- "outvar"

  nout <- length(mvfixed) # number of outcome measures, corresponding to the levels of `outcomevar`
  # Fit univariate GLM to each outcome to add linear predictor, residuals, weights
  data <- rlang::exec(
    rbind,
    !!!lapply(
      1:nout,
      function(j) fitglm(mvfixed[[j]], family[[j]],
                         data = data[data$outvar == levels(as.factor(data$outvar))[j], ])
    )
  )

  # capture call: `mcall` is used in the estimation step below;
  #               `Call` is for the output object
  mcall <- Call <- match.call()
  fixed <- mergeFormula(mvfixed)  # merged fixed-effects formula for multivariate response

  Call$fixed <- eval(fixed)
  if (!missing(random)) Call$random <- eval(random)

  ### Make data and functions available to fit multi-response glm.
  off <- 0
  data$zz <- data$eta + data$res - off # linear predictor + residual (+ offset not used)
  eta <- data$eta # current eta, used to assess convergence below

  nm <- names(mcall)[-1L]
  keep <- is.element(nm, c("fixed", "data", "na.action", "control"))
  for (i in nm[!keep]) mcall[[i]] <- NULL

  fixed[[2L]] <- quote(zz) # LHS
  mcall[["fixed"]] <- fixed # combined fixed effects part of the model
  mcall[[1L]] <- quote(nlme::lme.formula) # function
  if (!missing(random)) mcall$random <- random
  mcall$method <- method
  if (!missing(correlation)) mcall$correlation <- correlation
  mcall$weights <- weights
  data$invwt <- 1 / data$wz
  mcall$method <- method
  mcall$data <- data
  mcall$control <- control
  mcall$na.action <- na.action

  # Iterate until convergence
  for (i in seq_len(niter)) {
    if (verbose) message(gettextf("iteration %d", i), domain = NA)
    iter <- i
    fit <- eval(mcall)
    etaold <- eta
    eta <- stats::fitted(fit) + off
    data$eta <- eta
    # Convergence: (relative) tolerance should probably be parameterised
    if (sum((eta - etaold)^2) < 1e-06 * sum(eta^2)) break

    # call internal function to perform linearization step
    # first-order Taylor series approximation to the link response
    mle.fit <- list()
    for (j in 1:nout) mle.fit[[j]] <- extractlinkinv(j, data, family)
    mle.fit <- rlang::exec(rbind, !!!mle.fit)
    data$zz <- eta + (data$y0 - mle.fit$mu) / mle.fit$mu.eta.val - off
    data$ri <- (data$y0 - mle.fit$mu) / mle.fit$mu.eta.val
    data$invwt <- 1 / mle.fit$wz

    mcall$data <- data
  }

  attributes(fit$logLik) <- NULL
  fit$call <- Call
  fit$family <- family
  fit$logLik <- as.numeric(NA)
  fit$method <- method
  fit$iter <- iter
  oldClass(fit) <- c("mglmmPQL", oldClass(fit))

  fit
}
