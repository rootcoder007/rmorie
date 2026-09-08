% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cnvlfc_ccm}
\alias{cnvlfc_ccm}
\title{cnvlfc_ccm}
\usage{
cnvlfc_ccm(x, y, E = 2L, tau = 1L, lib_sizes = NULL, seed = 1L, exclude = 0L) 
}
\arguments{
\item{x}{Passed to \code{.cnvlfc_ccm}.}
\item{y}{Passed to \code{.cnvlfc_ccm}.}
\item{E}{Passed to \code{.cnvlfc_ccm}. Defaults to \code{2L}.}
\item{tau}{Passed to \code{.cnvlfc_ccm}. Defaults to \code{1L}.}
\item{lib_sizes}{Passed to \code{.cnvlfc_ccm}.}
\item{seed}{Passed to \code{.cnvlfc_ccm}. Defaults to \code{1L}.}
\item{exclude}{Passed to \code{.cnvlfc_ccm}. Defaults to \code{0L}.}
}
\description{
A step of the cnvlfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.cnvlfc_ccm}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cnvlfc_ccm(V, V)
}
