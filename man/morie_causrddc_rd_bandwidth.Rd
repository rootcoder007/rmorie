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
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{nu}{Defaults to \code{0}.}
\item{p}{Defaults to \code{1}.}
\item{kernel}{Defaults to \code{"triangular"}.}
\item{s}{Defaults to \code{0}.}
\item{prelim_order}{Defaults to \code{NULL}.}
}
\description{
Part of the causrddc_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{h}, \code{h_unclamped}, \code{at_bound}, \code{C}, \code{B}, \code{V}, \code{f}, \code{mu_plus}, \code{mu_minus}.
}
\keyword{internal}
