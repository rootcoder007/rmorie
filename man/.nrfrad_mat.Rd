% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nrfrad_mat}
\alias{.nrfrad_mat}
\title{S3 helper: ensure a numeric matrix of row vectors (k.mat)}
\usage{
.nrfrad_mat(colour)
}
\arguments{
\item{colour}{A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the nrfrad_native implementation. Called by \code{volume_render}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.nrfrad_mat(V)
}
