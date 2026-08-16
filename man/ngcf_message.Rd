% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ngcf_message}
\alias{ngcf_message}
\title{ngcf_message}
\usage{
ngcf_message(e_i, e_u, W1, W2, p_ui, affinity = TRUE)
}
\arguments{
\item{e_i}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{e_u}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{W1}{A matrix; indexed by row and column.}
\item{W2}{A matrix; indexed by row and column.}
\item{p_ui}{Numeric; combined arithmetically in the body.}
\item{affinity}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the ngcf_native implementation. Called by \code{ngcf_propagate}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
