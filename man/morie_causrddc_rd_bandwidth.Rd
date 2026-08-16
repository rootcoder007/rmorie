% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causrddc_rd_bandwidth}
\alias{morie_causrddc_rd_bandwidth}
\title{morie_causrddc_rd_bandwidth}
\usage{
morie_causrddc_rd_bandwidth(x, y, nu = 0, p = 1, kernel = "triangular", s = 0, prelim_order = NULL)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{y}{A vector; its length is taken.}
\item{nu}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{p}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{kernel}{Passed to \code{.causrddc_kernel_constants}. Defaults to \code{"triangular"}.}
\item{s}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{prelim_order}{Defaults to \code{NULL}.}
}
\description{
A step of the causrddc_native implementation. Called by \code{morie_causrddc}. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{h_unclamped}, \code{at_bound}, \code{C}, \code{B}, \code{V}, \code{f}, \code{mu_plus}, \code{mu_minus}.
}
\keyword{internal}
