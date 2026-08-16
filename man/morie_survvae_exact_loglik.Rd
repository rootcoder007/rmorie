% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_exact_loglik}
\alias{morie_survvae_exact_loglik}
\title{The true mixture log-likelihood the bound sits underneath.}
\usage{
morie_survvae_exact_loglik(X, y_lower, events, W, bias, shapes, scales, primitive = "weibull", alpha = 1)
}
\description{
Computes \eqn{\ln \sum_k g_k \exp(\cdot)} rather than the Jensen-relaxed sum. The anchor verifies this is at least as large as the ELBO from \code{morie_survvae_elbo} on every fit.
}
\value{
Named list with \code{loglik}, \code{uncensored},   \code{censored}.
}
\keyword{internal}
