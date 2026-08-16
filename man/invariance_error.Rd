% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{invariance_error}
\alias{invariance_error}
\title{invariance_error}
\usage{
invariance_error(energy_fn, R, Q, g = NULL)
}
\arguments{
\item{energy_fn}{Passed to \code{forces_from_energy}.}
\item{R}{Passed to \code{.schn_mat}.}
\item{Q}{Passed to \code{.schn_mat}.}
\item{g}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the schN_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{energy_error}, \code{force_error}, \code{energy_invariant}, \code{forces_equivariant}, \code{note}.
}
\keyword{internal}
