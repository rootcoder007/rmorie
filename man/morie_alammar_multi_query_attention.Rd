% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_multi_query_attention}
\alias{morie_alammar_multi_query_attention}
\title{morie_alammar_multi_query_attention}
\usage{
morie_alammar_multi_query_attention(Q_heads, K_shared, V_shared, n_query_heads) 
}
\arguments{
\item{K_shared,V_shared}{Single shared K and V matrices.}

\item{Q_heads}{Argument \code{Q_heads}; see Usage.}

\item{n_query_heads}{Argument \code{n_query_heads}; see Usage.}
}
\description{
morie_alammar_multi_query_attention
}
\value{
A list with \code{output}, \code{kv_cache_ratio}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
Qh <- lapply(1:4, function(i) matrix(stats::rnorm(6), 3, 2))
Ks <- matrix(stats::rnorm(6), 3, 2)
Vs <- matrix(stats::rnorm(6), 3, 2)
morie_alammar_multi_query_attention(Qh, Ks, Vs, 4)
}
