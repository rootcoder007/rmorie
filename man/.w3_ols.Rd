% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_ols}
\alias{.w3_ols}
\title{Least squares by the normal equations}
\usage{
.w3_ols(y, design)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{design}{A matrix; indexed by row and column.}
}
\description{
A step of the helpers_w3num implementation. Called by \code{morie_blinkg_scan}, \code{morie_blinkg_select}, \code{morie_sdcfst}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{rss}, \code{df}, \code{sigma2}, \code{xtx_inv}, \code{fitted}, \code{chol}.
}
\keyword{internal}
\examples{
.w3_ols(y = 5L, design = matrix(c(4, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3))
}
