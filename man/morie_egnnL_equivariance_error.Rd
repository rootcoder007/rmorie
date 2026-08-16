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
\item{H}{Passed to \code{run_egnn}.}
\item{X}{A vector; its length is taken and its elements indexed.}
\item{phi_e}{Passed to \code{run_egnn}.}
\item{phi_x}{Passed to \code{run_egnn}.}
\item{phi_h}{Passed to \code{run_egnn}.}
\item{Q}{A vector; indexed elementwise.}
\item{g}{A vector; indexed elementwise.}
\item{layers}{Passed to \code{run_egnn}. Defaults to \code{2}.}
\item{C}{Passed to \code{run_egnn}.}
}
\description{
A step of the egnnL_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{coordinate_error}, \code{feature_error}, \code{equivariant}, \code{invariant}, \code{note}.
}
\keyword{internal}
