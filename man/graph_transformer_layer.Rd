% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{graph_transformer_layer}
\alias{graph_transformer_layer}
\alias{graph_transformer}
\alias{graphtransformer}
\title{One graph transformer layer}
\usage{
graph_transformer_layer(H, adj, WQ, WK, WV, W1, W2, edge_bias = NULL, norm = "batch") 

graph_transformer(H, adj, WQ, WK, WV, W1, W2, edge_bias = NULL, norm = "batch") 

graphtransformer(H, adj, WQ, WK, WV, W1, W2, edge_bias = NULL, norm = "batch") 
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
\examples{
set.seed(1)
H <- matrix(rnorm(6), 3, 2)
adj <- list("0" = c("1" = 1, "2" = 1), "1" = c("0" = 1, "2" = 1),
            "2" = c("0" = 1, "1" = 1))
W <- matrix(rnorm(4), 2, 2)
W1 <- matrix(rnorm(8), 4, 2)
W2 <- matrix(rnorm(8), 2, 4)
graph_transformer_layer(H, adj, W, W, W, W1, W2)
}
