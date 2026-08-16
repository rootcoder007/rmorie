% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_estimate_aipw}
\alias{morie_estimate_aipw}
\title{morie_estimate_aipw}
\usage{
morie_estimate_aipw(data, treatment, outcome, covariates, propensity_col = NULL, outcome_model = c("linear", "logistic"), trim = c(0.01, 0.99), trim_type = "value", ps_model = "mle", ridge_lambda = 1, outcome_fit = "separate")
}
\arguments{
\item{data}{See Usage.}
\item{treatment}{See Usage.}
\item{outcome}{See Usage.}
\item{covariates}{See Usage.}
\item{propensity_col}{Defaults to \code{NULL}.}
\item{outcome_model}{Defaults to \code{c("linear", "logistic")}.}
\item{trim}{Defaults to \code{c(0.01, 0.99)}.}
\item{trim_type}{Defaults to \code{"value"}.}
\item{ps_model}{Defaults to \code{"mle"}.}
\item{ridge_lambda}{Defaults to \code{1}.}
\item{outcome_fit}{Defaults to \code{"separate"}.}
}
\description{
Part of the causal implementation; see the file header for the source it follows.
}
\value{
A list with \code{n_used}, \code{n_discarded}, \code{estimand}, \code{ate}, \code{se}, \code{ci_lower}, \code{ci_upper}, \code{n}.
}
\keyword{internal}
