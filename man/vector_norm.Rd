% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{vector_norm}
\alias{vector_norm}
\title{vector_norm}
\usage{
vector_norm(v)
}
\arguments{
\item{v}{Passed to \code{.painn_mat}.}
}
\description{
A step of the painn_native implementation. Called by \code{gated_update}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
vector_norm(V)
}
