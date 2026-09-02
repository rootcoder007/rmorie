% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_sliding_window_attention}
\alias{morie_alammar_sliding_window_attention}
\title{Sliding-window causal attention (Beltagy et al. 2020)}
\usage{
morie_alammar_sliding_window_attention(Q, K, V, window_size) 
}
\arguments{
\item{Q,K,V}{One sequence\'s matrices.}
\item{window_size}{W.}
}
\description{
Sliding-window causal attention (Beltagy et al. 2020)
}
\value{
A list with \code{output}, \code{attention}, \code{window}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
X <- matrix(stats::rnorm(6), 3, 2)
morie_alammar_sliding_window_attention(X, X, X, 2)
}
