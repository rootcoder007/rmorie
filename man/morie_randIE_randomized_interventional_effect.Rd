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
\item{Y}{Passed to \code{morie_randIE_interventional_mean}.}
\item{A}{Passed to \code{morie_randIE_interventional_mean}.}
\item{M}{Passed to \code{morie_randIE_interventional_mean}.}
\item{C}{Passed to \code{morie_randIE_interventional_mean}.}
\item{treated}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"1"}.}
\item{control}{Coerced to character by the body, with \code{as.character}. Defaults to \code{"0"}.}
\item{route}{Passed to \code{morie_randIE_interventional_mean}. Defaults to \code{"gformula"}.}
\item{laplace}{Passed to \code{morie_randIE_interventional_mean}. Defaults to \code{0}.}
}
\description{
A step of the randIE_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{total}, \code{direct}, \code{indirect}, \code{direct.control.arm}, \code{psi}, \code{route}, \code{treated}, \code{control}, \code{identity}, \code{method}.
}
\keyword{internal}
