% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_scaled_dot_attention}
\alias{morie_kamath_scaled_dot_attention}
\title{morie_kamath_scaled_dot_attention}
\usage{
morie_kamath_scaled_dot_attention(Q, K, V, d_k = NULL)
}
\arguments{
\item{Q,K,V}{Matrices.}
}
\description{
morie_kamath_scaled_dot_attention
}
\value{
A list with \code{output}, \code{attention}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_kamath_scaled_dot_attention(Q = 0.5, K = c(1, 2, 3, 4, 5, 6, 7, 8), V = c(1, 2, 3, 4, 5, 6, 7, 8))
}
