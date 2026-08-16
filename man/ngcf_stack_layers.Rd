% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ngcf_stack_layers}
\alias{ngcf_stack_layers}
\title{ngcf_stack_layers}
\usage{
ngcf_stack_layers(E0, adjacency, Ws, affinity = TRUE, slope = 0.2)
}
\arguments{
\item{E0}{A matrix; passed to \code{as.matrix}.}
\item{adjacency}{See Usage.}
\item{Ws}{A vector; its length is taken.}
\item{affinity}{Defaults to \code{TRUE}.}
\item{slope}{Defaults to \code{0.2}.}
}
\description{
A step of the ngcf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{final}, \code{layers}, \code{n_layers}, \code{affinity}, \code{method}, \code{note}.
}
\references{
----------
Wang, X., He, X., Wang, M., Feng, F. & Chua, T.-S. (2019) "Neural
Graph Collaborative Filtering", *Proceedings of the 42nd
International ACM SIGIR Conference on Research and Development in
Information Retrieval (SIGIR \'19)*, 165-174,
doi:10.1145/3331184.3331267. Sec. 1 (the collaborative signal is
absent from the embedding in conventional CF; the interpretation of
two- and three-layer propagation as behavioural similarity and
potential recommendations). Sec. 2.2.1 (message construction eq. (3)
including the e_i (*) e_u affinity term, the Laplacian coefficient
p_ui = 1/sqrt(|N_u||N_i|) read both as contribution and as a
path-length discount, and message aggregation eq. (4) with the
self-connection). Sec. 2.3 (concatenating the per-layer embeddings).

Kipf, T. N. & Welling, M. (2017) "Semi-Supervised Classification with
Graph Convolutional Networks", *ICLR 2017*, arXiv:1609.02907. The
graph convolution being extended.

He, X., Liao, L., Zhang, H., Nie, L., Hu, X. & Chua, T.-S. (2017)
"Neural Collaborative Filtering", *WWW \'17*, 173-182,
doi:10.1145/3038912.3052569. The framework NGCF is measured against;
implemented in :mod:`ncfRS`.
"""
}
\keyword{internal}
