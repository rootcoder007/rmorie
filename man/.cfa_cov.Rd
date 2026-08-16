% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.cfa_cov}
\alias{.cfa_cov}
\title{Item covariance from data, or pass a covariance matrix through}
\usage{
.cfa_cov(X)
}
\arguments{
\item{X}{Passed to \code{.s03mat}.}
}
\description{
A step of the cfafm2 implementation. Called by \code{Cfafm2}, \code{Cfaftr}. See the file header for the source the module follows.
}
\value{
The value of \code{S}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.cfa_cov(V)
}
