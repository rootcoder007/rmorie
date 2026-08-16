% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.painn_mat}
\alias{.painn_mat}
\title{.painn_mat}
\usage{
.painn_mat(M)
}
\arguments{
\item{M}{A matrix; the body checks with \code{is.matrix}.}
}
\description{
A step of the painn_native implementation. Called by \code{dipole_moment}, \code{gated_update}, \code{morie_painn_equivariance_error} and 2 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.painn_mat(V)
}
