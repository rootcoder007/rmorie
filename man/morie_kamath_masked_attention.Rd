% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_masked_attention}
\alias{morie_kamath_masked_attention}
\title{morie_kamath_masked_attention}
\usage{
morie_kamath_masked_attention(Q, K, V, M, d_k = NULL)
}
\arguments{
\item{M}{Additive mask, applied INSIDE the scaling (the book\'s   convention; differs from Vaswani\'s for finite masks).}
}
\description{
morie_kamath_masked_attention
}
\value{
A list with \code{output}, \code{attention}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_masked_attention(Q = 0.5, K = 5L, V = 5L, M = 5L)
}
