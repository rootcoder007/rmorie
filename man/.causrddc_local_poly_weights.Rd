% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causrddc_local_poly_weights}
\alias{.causrddc_local_poly_weights}
\title{.causrddc_local_poly_weights}
\usage{
.causrddc_local_poly_weights(x, h, p, nu, kernel = "triangular", side = 1)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{h}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; combined arithmetically in the body.}
\item{nu}{Numeric; combined arithmetically in the body.}
\item{kernel}{Passed to \code{.causrddc_kern}. Defaults to \code{"triangular"}.}
\item{side}{Defaults to \code{1}.}
}
\description{
A step of the causrddc_native implementation. Called by \code{morie_causrddc}. See the file header for the source the module follows.
}
\value{
A list with \code{w}, \code{omega}.
}
\keyword{internal}
