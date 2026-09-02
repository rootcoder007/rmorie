% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mistr_grouped_query_attention}
\alias{morie_mistr_grouped_query_attention}
\title{Grouped-query attention}
\usage{
morie_mistr_grouped_query_attention(Q, K, V, n_heads, n_kv_heads, mask = NULL, positions = NULL, 
    base = 10000) 
}
\arguments{
\item{Q,K,V}{Sequence matrices.}
\item{n_heads}{Query heads.}
\item{n_kv_heads}{Key-value heads (shared).}
\item{mask}{Optional L x L logical matrix.}
\item{positions}{Positions for RoPE; NULL uses 0..L-1, FALSE   disables RoPE.}
\item{base}{RoPE base.}
}
\description{
Grouped-query attention
}
\value{
An L x d matrix of per-token output.
}
\keyword{internal}
\examples{
set.seed(2)
L <- 4
Q <- matrix(rnorm(L * 8), L, 8)
K <- matrix(rnorm(L * 4), L, 4)
V <- matrix(rnorm(L * 4), L, 4)
r <- morie_mistr_grouped_query_attention(Q, K, V, n_heads = 2,
                                         n_kv_heads = 1)
str(r, max.level = 1)
}
