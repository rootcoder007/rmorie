% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_egnnL}
\alias{morie_egnnL}
\title{Morie entry point: matches the Python payload keys}
\usage{
morie_egnnL(H, X, layers, phi_e, phi_x, phi_h, A = NULL, C = NULL)
}
\arguments{
\item{H}{Passed to \code{run_egnn}.}
\item{X}{Passed to \code{run_egnn}.}
\item{layers}{Passed to \code{run_egnn}.}
\item{phi_e}{Passed to \code{run_egnn}.}
\item{phi_x}{Passed to \code{run_egnn}.}
\item{phi_h}{Passed to \code{run_egnn}.}
\item{A}{Passed to \code{run_egnn}.}
\item{C}{Passed to \code{run_egnn}.}
}
\description{
A step of the egnnL_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{run_egnn}.
}
\keyword{internal}
