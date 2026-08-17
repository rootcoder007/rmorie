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
\examples{
set.seed(1)
X <- lapply(1:20, function(i) rnorm(3))
tt <- rexp(20, 0.1)
ev <- rbinom(20, 1, 0.7)
W <- matrix(rnorm(9, 0, 0.3), 3, 3)
bias <- rep(0, 3)
r <- morie_survvae_exact_loglik(X, tt, ev, W, bias, shapes = c(1, 1.5, 2),
                                scales = c(5, 10, 15))
str(r, max.level = 1)
}
