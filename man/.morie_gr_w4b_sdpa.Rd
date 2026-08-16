% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_w4b_sdpa}
\alias{.morie_gr_w4b_sdpa}
\title{.morie_gr_w4b_sdpa}
\usage{
.morie_gr_w4b_sdpa(Q, K, V, mask = NULL)
}
\arguments{
\item{Q}{A matrix; passed to \code{ncol}.}
\item{K}{A matrix; passed to \code{t}.}
\item{V}{A matrix; passed to \code{\%*\%}.}
\item{mask}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the geron_w4b_native implementation. Called by \code{morie_geron_multihead_attention}. See the file header for the source the module follows.
}
\value{
A list with \code{output}, \code{attention}.
}
\keyword{internal}
\examples{
.morie_gr_w4b_sdpa(Q = 0.5, K = c(1, 2, 3, 4, 5, 6, 7, 8), V = c(1, 2, 3, 4, 5, 6, 7, 8))
}
