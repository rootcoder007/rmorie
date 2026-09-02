% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnsadt_loglik}
\alias{morie_bnsadt_loglik}
\title{Log-likelihood of the published estimates under the selection model}
\usage{
morie_bnsadt_loglik(x, sigma, mu, tau, family = "symmetric_step", params = numeric(0)) 
}
\arguments{
\item{x}{Published point estimates.}
\item{sigma}{Their standard errors.}
\item{mu}{Mean of the latent effect distribution.}
\item{tau}{Its standard deviation.}
\item{family}{A name of the selection family.}
\item{params}{The free publication probabilities.}
}
\description{
Log-likelihood of the published estimates under the selection model
}
\value{
The log-likelihood.
}
\keyword{internal}
\examples{
set.seed(1)
x <- rnorm(20, 0.3, 0.1)
sigma <- rep(0.1, 20)
morie_bnsadt_loglik(x, sigma, mu = 0.2, tau = 0.05,
                    family = "symmetric_step", params = 0.3)
}
