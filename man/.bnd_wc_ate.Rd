% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnd_wc_ate}
\alias{.bnd_wc_ate}
\title{.bnd_wc_ate}
\usage{
.bnd_wc_ate(yv, dv, lo, hi)
}
\arguments{
\item{yv}{Passed to \code{.bnd_cellmeans}.}
\item{dv}{Passed to \code{.bnd_cellmeans}.}
\item{lo}{Passed to \code{.bnd_wc_arm}.}
\item{hi}{Passed to \code{.bnd_wc_arm}.}
}
\description{
A step of the helpers_bnd implementation. Called by \code{Bndtfm}, \code{Bnscbo}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
.bnd_wc_ate(yv = c(1, 2, 3, 4, 5, 6, 7, 8), dv = c(1, 2, 3, 4, 5, 6, 7, 8), lo = c(1, 2, 3, 4, 5, 6, 7, 8), hi = c(1, 2, 3, 4, 5, 6, 7, 8))
}
