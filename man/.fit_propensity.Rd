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
\item{data}{A vector; indexed elementwise.}
\item{treatment}{See Usage.}
\item{covariates}{Passed to \code{.mor_ps_design}.}
\item{ps_model}{One of \code{"mle"}, \code{"ridge"}. Defaults to \code{"mle"}.}
\item{ridge_lambda}{Defaults to \code{1}.}
}
\description{
A step of the causal implementation. Called by \code{.fit_propensity_weightit}, \code{morie_estimate_propensity_scores}, \code{morie_weight_ow} and 1 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
