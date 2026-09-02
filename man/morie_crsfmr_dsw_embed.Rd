% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crsfmr_dsw_embed}
\alias{morie_crsfmr_dsw_embed}
\title{Dimension-Segment-Wise embedding}
\usage{
morie_crsfmr_dsw_embed(X, seg_len, E = NULL, pos = NULL) 
}
\arguments{
\item{X}{Numeric matrix, T rows by D columns.}
\item{seg_len}{Integer, segment length.}
\item{E}{Optional embedding matrix of shape d_model by seg_len.}
\item{pos}{Optional positional embedding array.}
}
\description{
Eq. (1)-(2): segment each dimension, then embed each segment. The output is a 3D array indexed as \code{H[[i, d]]} for the i-th segment of dimension d.
}
\value{
A list with \code{H}, \code{n_seg}, \code{D}, \code{d_model},   \code{seg_len} and \code{shape}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(rnorm(40), 20, 2)
morie_crsfmr_dsw_embed(X, seg_len = 4)
}
