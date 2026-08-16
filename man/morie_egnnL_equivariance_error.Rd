% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_egnnL_equivariance_error}
\alias{morie_egnnL_equivariance_error}
\title{morie_egnnL_equivariance_error}
\usage{
morie_egnnL_equivariance_error(H, X, phi_e, phi_x, phi_h, Q, g, layers = 2, C = NULL)
}
\arguments{
\item{H}{See Usage.}
\item{X}{A vector; its length is taken and its elements indexed.}
\item{phi_e}{See Usage.}
\item{phi_x}{See Usage.}
\item{phi_h}{See Usage.}
\item{Q}{A vector; indexed elementwise.}
\item{g}{A vector; indexed elementwise.}
\item{layers}{Defaults to \code{2}.}
\item{C}{Defaults to \code{NULL}.}
}
\description{
A step of the egnnL_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{coordinate_error}, \code{feature_error}, \code{equivariant}, \code{invariant}, \code{note}.
}
\keyword{internal}
