% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Cclogl}
\alias{Cclogl}
\title{Case-control logistic likelihood (Lawson eq. 6.6)}
\usage{
Cclogl(eta, y)
}
\description{
Case-control logistic likelihood (Lawson eq. 6.6)
}
\value{
A list with \code{loglik}, \code{p}, \code{n_cases}, \code{n}.
}
\keyword{internal}
\examples{
set.seed(4)
Cclogl(eta = rnorm(8), y = rbinom(8, 1, 0.5))
}
