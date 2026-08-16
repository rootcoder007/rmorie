% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Postres}
\alias{Postres}
\title{Posterior-averaged Poisson residual (Lawson eq. 5.2)}
\usage{
Postres(y, e, theta_draws)
}
\description{
Posterior-averaged Poisson residual (Lawson eq. 5.2)
}
\value{
A list with \code{residual}, \code{fitted}, \code{n}, \code{n_draws}.
}
\keyword{internal}
\examples{
set.seed(1)
e <- runif(20, 5, 15)
theta <- matrix(rgamma(80, 2, 2), 4, 20)
y <- rpois(20, e * colMeans(theta))
Postres(y, e, theta)
}
