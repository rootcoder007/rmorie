% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Resdist}
\alias{Resdist}
\title{Resistance distance matrix (Klein & Randic 1993)}
\usage{
Resdist(A, tol = 1e-09) 
}
\description{
Resistance distance matrix (Klein & Randic 1993)
}
\arguments{
\item{A}{Argument \code{A}; see Usage.}
\item{tol}{Argument \code{tol}; see Usage.}
}
\value{
A list with \code{R}, \code{Lplus}, \code{n}, \code{rank}.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
rmorie:::Resdist(A)
}
