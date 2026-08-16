% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dipole_moment}
\alias{dipole_moment}
\title{dipole_moment}
\usage{
dipole_moment(charges, R, centre = NULL)
}
\arguments{
\item{charges}{Passed to \code{.painn_vec}.}
\item{R}{Passed to \code{.painn_mat}.}
\item{centre}{Optional; may be \code{NULL}. Passed to \code{.painn_vec}.}
}
\description{
A step of the painn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{dipole}, \code{magnitude}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
dipole_moment(V, V)
}
