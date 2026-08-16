% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schn_mat}
\alias{.schn_mat}
\title{.schn_mat}
\usage{
.schn_mat(x)
}
\arguments{
\item{x}{A matrix; the body checks with \code{is.matrix}.}
}
\description{
A step of the schN_native implementation. Called by \code{cfconv}, \code{forces_from_energy}, \code{invariance_error}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.schn_mat(M)
}
