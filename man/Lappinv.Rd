% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lappinv}
\alias{Lappinv}
\title{Graph Laplacian pseudoinverse (Klein & Randic 1993)}
\usage{
Lappinv(A, tol = 1e-09) 
}
\description{
Graph Laplacian pseudoinverse (Klein & Randic 1993)
}
\arguments{
\item{A}{Argument \code{A}; see Usage.}
\item{tol}{Argument \code{tol}; see Usage.}
}
\value{
A list with \code{Lplus}, \code{eigenvalues}, \code{rank}, \code{n}.
}
\keyword{internal}
\examples{
A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
rmorie:::Lappinv(A)
}
