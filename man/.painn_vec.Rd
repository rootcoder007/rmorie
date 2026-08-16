% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.painn_vec}
\alias{.painn_vec}
\title{.painn_vec}
\usage{
.painn_vec(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
A step of the painn_native implementation. Called by \code{dipole_moment}, \code{gated_update}, \code{scalar_vector_message}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.painn_vec(V)
}
