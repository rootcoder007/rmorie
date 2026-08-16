% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_gtrf_sparse_attention}
\alias{morie_gtrf_sparse_attention}
\title{Graph transformer: attention that respects the graph}
\usage{
morie_gtrf_sparse_attention(H, adj, WQ, WK, WV, edge_bias = NULL)
}
\description{
Graph transformer: attention that respects the graph. Transformer Networks to Graphs, AAAI Workshop on Deep Learning on Graphs (arXiv:2012.09699) -- neighbour-restricted attention, Laplacian-eigenvector positional encoding, batch normalisation instead of layer normalisation, and the edge-feature pipeline; the architecture being generalised; Belkin, M. and Niyogi, M. Representation, Neural Computation 15(6) -- the eigenvectors used as the positional encoding. Native implementation mirroring Python morie.fn.gtrf exactly: the same normalised Laplacian L = I - D^\{-1/2\} A D^\{-1/2\}, the same k smallest non-trivial eigenvectors as positional encoding, the same random sign flip during training, the same neighbour-restricted softmax attention with optional edge bias, and the same attention-residual-norm, feed-forward-residual-norm block.
}
\value{
A list with components \code{nrow}, \code{ncol}, \code{drop}, \code{output}, \code{note}.
}
\references{
Sources: Dwivedi, V. P. and Bresson, X. (2020), A Generalization of
Vaswani, A. et al. (2017), Attention Is All You Need, NIPS 2017 --
(2003), Laplacian Eigenmaps for Dimensionality Reduction and Data
}
\keyword{internal}
