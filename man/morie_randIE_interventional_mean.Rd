% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_randIE_interventional_mean}
\alias{morie_randIE_interventional_mean}
\title{morie_randIE_interventional_mean}
\usage{
morie_randIE_interventional_mean(Y, A, M, C = NULL, a = "1", a.star = "0", route = "gformula", laplace = 0)
}
\arguments{
\item{Y}{See Usage.}
\item{A}{Passed to \code{.labels}.}
\item{M}{Passed to \code{.labels}.}
\item{C}{Optional; may be \code{NULL}. Passed to \code{.labels}.}
\item{a}{Defaults to \code{"1"}.}
\item{a.star}{Defaults to \code{"0"}.}
\item{route}{Compared against \code{"weighting"}. Defaults to \code{"gformula"}.}
\item{laplace}{Passed to \code{morie_randIE_mediator_distribution}. Defaults to \code{0}.}
}
\description{
A step of the randIE_native implementation. Called by \code{morie_randIE_randomized_interventional_effect}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{a}, \code{a.star}, \code{route}, \code{own.mediator.mean}, \code{n.arm}, \code{n}, \code{note}.
}
\keyword{internal}
