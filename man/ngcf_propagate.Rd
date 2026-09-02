% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ngcf_propagate}
\alias{ngcf_propagate}
\title{ngcf_propagate}
\usage{
ngcf_propagate(E, adjacency, W1, W2, affinity = TRUE, slope = 0.2) 
}
\arguments{
\item{E}{A matrix; indexed by row and column.}
\item{adjacency}{A vector; indexed elementwise.}
\item{W1}{Passed to \code{ngcf_message}.}
\item{W2}{Passed to \code{ngcf_message}.}
\item{affinity}{Passed to \code{ngcf_message}. Defaults to \code{TRUE}.}
\item{slope}{Passed to \code{.ngcf_leaky}. Defaults to \code{0.2}.}
}
\description{
A step of the ngcf_native implementation. Called by \code{ngcf_stack_layers}. See the file header for the source the module follows.
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
\examples{
set.seed(1)
d <- 3
E <- matrix(rnorm(12), 4, 3)
adjacency <- list(c(2, 4), c(1, 3), c(2, 4), c(1, 3))
W1 <- matrix(rnorm(d * d, 0, 0.3), d, d)
W2 <- matrix(rnorm(d * d, 0, 0.3), d, d)
Z <- ngcf_propagate(E, adjacency, W1, W2)
dim(Z)
}
