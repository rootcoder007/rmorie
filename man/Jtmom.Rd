% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Jtmom}
\alias{Jtmom}
\title{JT null moments -- eqs. (10.6.2)-(10.6.3), pp. 365-366}
\usage{
Jtmom(ns) 
}
\description{
JT null moments -- eqs. (10.6.2)-(10.6.3), pp. 365-366
}
\arguments{
\item{ns}{Argument \code{ns}; see Usage.}
}
\value{
A list with \code{mean}, \code{mean_pairwise}, \code{var}, \code{sd}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Jtmom(V)
}
