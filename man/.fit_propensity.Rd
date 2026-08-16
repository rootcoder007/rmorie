% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fit_propensity}
\alias{.fit_propensity}
\title{.fit_propensity}
\usage{
.fit_propensity(data, treatment, covariates, ps_model = "mle", ridge_lambda = 1)
}
\arguments{
\item{data}{See Usage.}
\item{treatment}{See Usage.}
\item{covariates}{See Usage.}
\item{ps_model}{Defaults to \code{"mle"}.}
\item{ridge_lambda}{Defaults to \code{1}.}
}
\description{
Part of the causal implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
