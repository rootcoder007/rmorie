% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Likprod}
\alias{Likprod}
\title{Joint likelihood of independent observations (Lawson eq. 3.1)}
\usage{
Likprod(dens)
}
\description{
Joint likelihood of independent observations (Lawson eq. 3.1)
}
\value{
A list with \code{likelihood}, \code{loglik}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Likprod(V)
}
