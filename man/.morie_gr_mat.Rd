% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_mat}
\alias{.morie_gr_mat}
\title{.morie_gr_mat}
\usage{
.morie_gr_mat(x, name)
}
\arguments{
\item{x}{A matrix; passed to \code{as.matrix}.}
\item{name}{See Usage.}
}
\description{
A step of the geron_ml_native implementation. Called by \code{.morie_gr_lsa}, \code{.morie_gr_w4b_grid_search}, \code{.morie_w4c_centroid_pair} and 82 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{m}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_mat(V, V)
}
