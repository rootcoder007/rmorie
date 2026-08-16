% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_estimate_propensity_scores}
\alias{morie_estimate_propensity_scores}
\title{morie_estimate_propensity_scores}
\usage{
morie_estimate_propensity_scores(data, treatment, covariates, trim = c(0.01, 0.99), trim_type = "value", ps_model = "mle", ridge_lambda = 1)
}
\arguments{
\item{data}{See Usage.}
\item{treatment}{See Usage.}
\item{covariates}{See Usage.}
\item{trim}{Defaults to \code{c(0.01, 0.99)}.}
\item{trim_type}{Defaults to \code{"value"}.}
\item{ps_model}{Defaults to \code{"mle"}.}
\item{ridge_lambda}{Defaults to \code{1}.}
}
\description{
Part of the causal implementation; see the file header for the source it follows.
}
\value{
The value of \code{.mor_trim_ps}.
}
\keyword{internal}
