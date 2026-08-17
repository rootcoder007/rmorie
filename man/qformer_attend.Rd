% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{qformer_attend}
\alias{qformer_attend}
\title{qformer_attend}
\usage{
qformer_attend(queries, image_features, WQ, WK, WV)
}
\arguments{
\item{queries}{A matrix; passed to \code{as.matrix}.}
\item{image_features}{A matrix; passed to \code{as.matrix}.}
\item{WQ}{A matrix; passed to \code{nrow}.}
\item{WK}{Passed to \code{proj}.}
\item{WV}{A matrix; passed to \code{ncol}.}
}
\description{
A step of the blip2v_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{output}, \code{weights}, \code{n_queries}, \code{n_patches}, \code{compression}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
Q <- matrix(rnorm(8), 2, 4)
Im <- matrix(rnorm(12), 3, 4)
W <- matrix(rnorm(16), 4, 4)
qformer_attend(Q, Im, W, W, W)
}
