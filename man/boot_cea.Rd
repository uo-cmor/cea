% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot.R
\name{boot_cea}
\alias{boot_cea}
\title{Bootstrap resampling of CEA estimates}
\usage{
boot_cea(
  x,
  R,
  estimand = "ATE",
  sim = "parametric",
  weights = NULL,
  simple = FALSE,
  parallel = c("no", "multicore", "snow"),
  ncpus = getOption("cea.boot.ncpus", 1L),
  cl = NULL
)
}
\arguments{
\item{x}{\code{cea_estimate} object. The fitted CEA regression model.}

\item{R}{The number of bootstrap replicates.}

\item{estimand}{String scalar. Whether to calculate the average treatment
effect (ATE), average treatment effect on the treated (ATT), or average
treatment effect on the controls (ATC). Only used for non-linear models.}

\item{sim}{A character vector indicating the type of simulation required.
Possible values are "ordinary" (the default), "parametric", "balanced",
or "permutation".}

\item{weights, simple, parallel, ncpus, cl}{Passed to \verb{\link[boot]\{boot\}}. For
\code{parallel} and \code{ncpus}, default values are taken from
\code{getOption(cea.boot.parallel)} and \code{getOption(cea.boot.ncpus)} instead
of their \code{boot}-package equivalents.}
}
\description{
Generate R bootstrap replicates of mean incremental QALYs and Costs from a
fitted CEA regression model, using the \code{boot} package.
}
