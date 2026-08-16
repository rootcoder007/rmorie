% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_egcn}
\alias{morie_egcn}
\title{Morie entry point: matches the Python payload keys}
\usage{
morie_egcn(H, X, layers, phi_e, phi_x, phi_h, A = NULL, C = NULL)
}
\arguments{
\item{H}{Carried through into a list the body builds.}
\item{X}{Carried through into a list the body builds.}
\item{layers}{Coerced to integer by the body, with \code{as.integer}.}
\item{phi_e}{See Usage.}
\item{phi_x}{See Usage.}
\item{phi_h}{See Usage.}
\item{A}{Defaults to \code{NULL}.}
\item{C}{Defaults to \code{NULL}.}
}
\description{
A step of the egcn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{H}, \code{X}, \code{layers}, \code{method}, \code{note}.
}
\keyword{internal}
