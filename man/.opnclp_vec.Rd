% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.opnclp_vec}
\alias{.opnclp_vec}
\title{.opnclp_vec}
\usage{
.opnclp_vec(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
A step of the opnclp_native implementation. Called by \code{fit_power_law}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.opnclp_vec(V)
}
