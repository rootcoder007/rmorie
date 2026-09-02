% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_predict_survival}
\alias{morie_survvae_predict_survival}
\title{\eqn{S(t | x)} as the gated mixture of expert survivals}
\usage{
morie_survvae_predict_survival(fit_result, x, times) 
}
\arguments{
\item{fit_result}{A fit returned by \code{morie_survvae} or   \code{morie_survvae_fit_competing}.}
\item{x}{Covariate vector.}
\item{times}{Numeric vector of times to evaluate.}
}
\description{
\eqn{S(t | x)} as the gated mixture of expert survivals.
}
\value{
Named list with \code{time}, \code{survival}, \code{gates}.
}
\keyword{internal}
\examples{
set.seed(2)
X <- lapply(1:40, function(i) rnorm(2))
tt <- rexp(40, 0.1)
ev <- rbinom(40, 1, 0.7)
fit <- morie_survvae(X, tt, ev, K = 2L, restarts = 2L)
morie_survvae_predict_survival(fit, rnorm(2), times = c(5, 10, 20))
}
