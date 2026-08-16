% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{graph_transformer_layer}
\alias{graph_transformer_layer}
\title{One graph transformer layer}
\usage{
graph_transformer_layer(H, adj, WQ, WK, WV, W1, W2, edge_bias = NULL, norm = "batch")
}
\arguments{
\item{H}{Node feature matrix.}
\item{adj}{Adjacency list.}
\item{WQ,WK,WV}{Attention projection matrices.}
\item{W1,W2}{Feed-forward matrices.}
\item{edge_bias}{Optional list of edge biases keyed by (i, j).}
\item{norm}{One of "batch", "layer", "none".}
}
\description{
One graph transformer layer
}
\value{
Updated node feature matrix.
}
\keyword{internal}
