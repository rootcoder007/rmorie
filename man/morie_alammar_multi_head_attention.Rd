% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_multi_head_attention}
\alias{morie_alammar_multi_head_attention}
\title{Multi-head attention (Vaswani et al. 2017)}
\usage{
morie_alammar_multi_head_attention(Q, K, V, Wq, Wk, Wv, Wo, heads)
}
\arguments{
\item{Q,K,V}{Matrices.}
\item{Wq,Wk,Wv}{Lists of matrices.}
\item{Wo}{Output projection.}
\item{heads}{Head count.}
}
\description{
Concat(head_1..head_h) W_O; Wq, Wk, Wv are LISTS of per-head projections -- one per head, refused otherwise, since broadcasting one matrix would compute h copies of the same head.
}
\value{
A list with \code{output}, \code{heads}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
