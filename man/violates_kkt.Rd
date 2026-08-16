% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{violates_kkt}
\alias{violates_kkt}
\title{violates_kkt}
\usage{
violates_kkt(i, alpha, y, E, C, tol = 0.001)
}
\arguments{
\item{i}{See Usage.}
\item{alpha}{See Usage.}
\item{y}{See Usage.}
\item{E}{See Usage.}
\item{C}{See Usage.}
\item{tol}{Defaults to \code{0.001}.}
}
\description{
Part of the smoopt_native implementation; see the file header for the source it follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
violates_kkt(i = 5L, alpha = 0.5, y = c(1, 2, 3, 4, 5, 6, 7, 8), E = c(1, 2, 3, 4, 5, 6, 7, 8), C = c(1, 2, 3, 4, 5, 6, 7, 8))
}
