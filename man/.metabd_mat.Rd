% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.metabd_mat}
\alias{.metabd_mat}
\title{.metabd_mat}
\usage{
.metabd_mat(X)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
}
\description{
A step of the metabd_native implementation. Called by \code{bin_contigs}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.metabd_mat(V)
}
