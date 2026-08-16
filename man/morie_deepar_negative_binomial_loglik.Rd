% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_deepar_negative_binomial_loglik}
\alias{morie_deepar_negative_binomial_loglik}
\title{Negative binomial log-likelihood}
\usage{
morie_deepar_negative_binomial_loglik(z, mu, alpha)
}
\arguments{
\item{z}{Numeric observation.}
\item{mu}{Numeric mean.}
\item{alpha}{Numeric overdispersion.}
}
\description{
Variance is \code{mu (1 + mu alpha)}, so \code{alpha} IS the overdispersion and \code{alpha -> 0} recovers Poisson.
}
\value{
Numeric log-density.
}
\keyword{internal}
\examples{
morie_deepar_negative_binomial_loglik(z = 5L, mu = c(1, 2, 3, 4, 5, 6, 7, 8), alpha = 0.5)
}
