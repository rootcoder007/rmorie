% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_deepar_gaussian_loglik}
\alias{morie_deepar_gaussian_loglik}
\title{Gaussian log-likelihood}
\usage{
morie_deepar_gaussian_loglik(z, mu, sigma) 
}
\arguments{
\item{z}{Numeric observation.}
\item{mu}{Numeric mean.}
\item{sigma}{Numeric standard deviation.}
}
\description{
For real-valued series.
}
\value{
Numeric log-density.
}
\keyword{internal}
\examples{
morie_deepar_gaussian_loglik(z = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8),
  sigma = 0.5)
}
