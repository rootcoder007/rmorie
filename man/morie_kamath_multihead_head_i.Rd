% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_multihead_head_i}
\alias{morie_kamath_multihead_head_i}
\title{morie_kamath_multihead_head_i}
\usage{
morie_kamath_multihead_head_i(Q, K, V, W_Qi, W_Ki, W_Vi)
}
\arguments{
\item{W_Qi,W_Ki,W_Vi}{Per-head projections.}
}
\description{
morie_kamath_multihead_head_i
}
\value{
A list with \code{head}, \code{attention}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
X <- matrix(stats::rnorm(6), 3, 2)
W <- lapply(1:3, function(i) matrix(stats::rnorm(4), 2, 2))
morie_kamath_multihead_head_i(X, X, X, W[[1]], W[[2]], W[[3]])
}
