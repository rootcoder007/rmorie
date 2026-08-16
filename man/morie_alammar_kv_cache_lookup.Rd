% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_kv_cache_lookup}
\alias{morie_alammar_kv_cache_lookup}
\title{KV-cache append plus one attention row (Alammar Ch 3)}
\usage{
morie_alammar_kv_cache_lookup(K_cache, V_cache, k_new, v_new, q_new)
}
\arguments{
\item{K_cache,V_cache}{Existing caches or NULL.}
\item{k_new,v_new,q_new}{Single-row matrices.}
}
\description{
KV-cache append plus one attention row (Alammar Ch 3)
}
\value{
A list with \code{output}, \code{attention}, \code{K_cache}, \code{V_cache}, \code{cache_length}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
