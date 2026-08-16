% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnd_cellmeans}
\alias{.bnd_cellmeans}
\title{.bnd_cellmeans}
\usage{
.bnd_cellmeans(yv, dv)
}
\arguments{
\item{yv}{A vector; its length is taken and its elements indexed.}
\item{dv}{Passed to \code{==}.}
}
\description{
A step of the helpers_bnd implementation. Called by \code{.bnd_wc_ate}, \code{Bndngt}, \code{Bndnvg} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{p1}, \code{m1}, \code{p0}, \code{m0}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bnd_cellmeans(V, V)
}
