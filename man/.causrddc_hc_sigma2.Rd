% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causrddc_hc_sigma2}
\alias{.causrddc_hc_sigma2}
\title{.causrddc_hc_sigma2}
\usage{
.causrddc_hc_sigma2(x, y, h, p, kernel)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{y}{A vector; indexed elementwise.}
\item{h}{Numeric; combined arithmetically in the body.}
\item{p}{Numeric; combined arithmetically in the body.}
\item{kernel}{Passed to \code{.causrddc_kern}.}
}
\description{
A step of the causrddc_native implementation. Called by \code{morie_causrddc}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
