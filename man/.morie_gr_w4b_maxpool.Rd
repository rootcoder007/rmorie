% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_w4b_maxpool}
\alias{.morie_gr_w4b_maxpool}
\title{.morie_gr_w4b_maxpool}
\usage{
.morie_gr_w4b_maxpool(x, k, s)
}
\arguments{
\item{x}{A matrix; indexed by row and column.}
\item{k}{Numeric; combined arithmetically in the body.}
\item{s}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the geron_w4b_native implementation. Called by \code{morie_geron_max_pool}. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{argmax}, \code{output_shape}.
}
\keyword{internal}
\examples{
.morie_gr_w4b_maxpool(x = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), k = 3L, s = c(1, 2, 3, 4, 5, 6, 7, 8))
}
