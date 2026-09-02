% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ginagg}
\alias{Ginagg}
\title{Graph isomorphism network aggregation (Xu et al. 2019)}
\usage{
Ginagg(A, H, eps = 0) 
}
\description{
Graph isomorphism network aggregation (Xu et al. 2019)
}
\arguments{
\item{A}{Argument \code{A}; see Usage.}
\item{H}{Argument \code{H}; see Usage.}
\item{eps}{Argument \code{eps}; see Usage.}
}
\value{
A list with \code{H}, \code{eps}, \code{n_nodes}, \code{dim}.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
H <- matrix(1:6, 3, 2)
rmorie:::Ginagg(A, H, eps = 0.1)
}
