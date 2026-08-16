% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_randIE_randomized_interventional_effect}
\alias{morie_randIE_randomized_interventional_effect}
\title{morie_randIE_randomized_interventional_effect}
\usage{
morie_randIE_randomized_interventional_effect(Y, A, M, C = NULL, treated = "1", control = "0", route = "gformula", laplace = 0)
}
\arguments{
\item{Y}{See Usage.}
\item{A}{See Usage.}
\item{M}{See Usage.}
\item{C}{Defaults to \code{NULL}.}
\item{treated}{Defaults to \code{"1"}.}
\item{control}{Defaults to \code{"0"}.}
\item{route}{Defaults to \code{"gformula"}.}
\item{laplace}{Defaults to \code{0}.}
}
\description{
Part of the randIE_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{total}, \code{direct}, \code{indirect}, \code{direct.control.arm}, \code{psi}, \code{route}, \code{treated}, \code{control}, \code{identity}, \code{method}.
}
\keyword{internal}
