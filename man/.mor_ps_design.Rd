% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ps_design}
\alias{.mor_ps_design}
\title{.mor_ps_design}
\usage{
.mor_ps_design(data, covariates)
}
\arguments{
\item{data}{A vector; indexed elementwise.}
\item{covariates}{Iterated over elementwise, with \code{lapply}.}
}
\description{
A step of the causal implementation. Called by \code{.fit_propensity}, \code{morie_estimate_aipw}. See the file header for the source the module follows.
}
\value{
The value of \code{cbind}.
}
\keyword{internal}
