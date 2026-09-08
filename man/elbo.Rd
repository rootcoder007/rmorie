% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{elbo}
\alias{elbo}
\title{Monte Carlo ELBO}
\usage{
elbo(log_joint, log_q, samples) 
}
\arguments{
\item{log_joint,log_q}{Functions of a sample, each returning a   numeric scalar.}
\item{samples}{List of samples.}
}
\description{
\code{E_q[log p(x,z) - log q(z)]}. The common yardstick: both approaches optimise it, so a deeper flow must not lower it. The variance uses the (n-1) denominator so both arms agree to the last bit when the same samples are passed in.
}
\value{
A list with \code{elbo, se, n_samples, note}.
}
\keyword{internal}
\examples{
lj <- function(s) -0.5 * s^2 - 1
lq <- function(s) -0.5 * (s - 0.2)^2 - 0.9
r <- elbo(lj, lq, samples = c(-0.5, 0, 0.5, 1))
str(r, max.level = 1)
}
