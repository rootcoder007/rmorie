% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_grouped_query_attention}
\alias{morie_alammar_grouped_query_attention}
\title{Grouped-query and multi-query attention (Ainslie et al. 2023;}
\usage{
morie_alammar_grouped_query_attention(Q_heads, K_groups, V_groups, n_query_heads, n_kv_groups)
}
\arguments{
\item{Q_heads}{List of per-head query matrices.}
\item{K_groups,V_groups}{Lists of shared K and V.}
\item{n_query_heads,n_kv_groups}{Counts.}
}
\description{
Shazeer 2019) head_i = Attn(Q_i, `K_{g(i)}, V_{g(i)}`) with g(i) = i mod G. G = 1 recovers MQA; G = H full multi-head. H must divide by G.
}
\value{
A list with \code{output}, \code{group_assignment}, \code{kv_cache_ratio}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
