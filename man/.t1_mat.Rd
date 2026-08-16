% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t1_mat}
\alias{.t1_mat}
\title{.t1_mat}
\usage{
.t1_mat(X)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the tail1_core implementation. Called by \code{.ecfp_bonds}, \code{Admmlasso}, \code{Advielbo} and 45 others in the module. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t1_mat(V)
}
