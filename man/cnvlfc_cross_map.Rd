% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cnvlfc_cross_map}
\alias{cnvlfc_cross_map}
\title{cnvlfc_cross_map}
\usage{
cnvlfc_cross_map(driver, response, E = 2L, tau = 1L, library = NULL, seed = 1L, exclude = 0L)
}
\arguments{
\item{driver}{Passed to \code{.cnvlfc_cross_map}.}
\item{response}{Passed to \code{.cnvlfc_cross_map}.}
\item{E}{Passed to \code{.cnvlfc_cross_map}. Defaults to \code{2L}.}
\item{tau}{Passed to \code{.cnvlfc_cross_map}. Defaults to \code{1L}.}
\item{library}{Passed to \code{.cnvlfc_cross_map}.}
\item{seed}{Passed to \code{.cnvlfc_cross_map}. Defaults to \code{1L}.}
\item{exclude}{Passed to \code{.cnvlfc_cross_map}. Defaults to \code{0L}.}
}
\description{
A step of the cnvlfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{.cnvlfc_cross_map}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cnvlfc_cross_map(V, V)
}
