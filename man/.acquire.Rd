% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.acquire}
\alias{.acquire}
\title{Acquisition dispatch}
\usage{
.acquire(mu, sd, f_best, acq = "ei", kappa = 2, xi = 0)
}
\description{
Acquisition dispatch
}
\value{
The value of \code{switch}.
}
\keyword{internal}
\examples{
.acquire(mu = c(1, 2, 3, 4, 5, 6, 7, 8), sd = c(1, 2, 3, 4, 5, 6, 7, 8), f_best = c(1, 2, 3, 4, 5, 6, 7, 8))
}
