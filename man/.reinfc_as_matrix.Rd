% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reinfc_as_matrix}
\alias{.reinfc_as_matrix}
\title{.reinfc_as_matrix}
\usage{
.reinfc_as_matrix(x, name)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. A matrix; passed to \code{dim}.}
\item{name}{Passed to \code{sprintf}.}
}
\description{
A step of the reinfc_native implementation. Called by \code{morie_reinfc}. See the file header for the source the module follows.
}
\value{
The value of \code{x_mat}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.reinfc_as_matrix(V, V)
}
