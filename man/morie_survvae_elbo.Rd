% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_elbo}
\alias{morie_survvae_elbo}
\title{The paper\'s lower bound: gates outside the logarithm.}
\usage{
morie_survvae_elbo(X, y_lower, events, W, bias, shapes, scales, primitive = "weibull", alpha = 1, prior = 0)
}
\arguments{
\item{X}{List of covariate vectors, length \code{n}.}
\item{y_lower}{Numeric vector of observation times, length   \code{n}.}
\item{events}{Event indicator, length \code{n} (truthy =   uncensored).}
\item{W}{List of \code{K} weight vectors of length \code{d}.}
\item{bias}{Numeric vector of length \code{K}.}
\item{shapes}{Numeric vector of \code{K} positive shape   parameters.}
\item{scales}{Numeric vector of \code{K} positive scale   parameters.}
\item{primitive}{One of \code{"weibull"} or \code{"lognormal"}.}
\item{alpha}{Discount on the censored term, in \code{[0, 1]}.}
\item{prior}{Strength of the log-parameter L2 penalty.}
}
\description{
ELBO_U + \code{alpha} * ELBO_C - prior_penalty, where each term uses the gate weights OUTSIDE the log (Jensen). Verifying that this bound is less than or equal to the true mixture log- likelihood is a built-in sanity check (see \code{morie_survvae_exact_loglik}).
}
\value{
Named list with \code{elbo}, \code{uncensored},   \code{censored}, \code{prior_penalty}, \code{alpha}.
}
\keyword{internal}
