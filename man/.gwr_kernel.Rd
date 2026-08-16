% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gwr_kernel}
\alias{.gwr_kernel}
\title{.gwr_kernel}
\usage{
.gwr_kernel(d, h, kernel)
}
\arguments{
\item{d}{A vector; its length is taken.}
\item{h}{Numeric; combined arithmetically in the body.}
\item{kernel}{One of \code{"bisquare"}, \code{"gaussian"}, \code{"tricube"}.}
}
\description{
A step of the gwrcal_native implementation. Called by \code{.gwr_cv_score}, \code{.gwr_fit}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
