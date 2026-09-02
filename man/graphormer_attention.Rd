% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{graphormer_attention}
\alias{graphormer_attention}
\alias{graphormer}
\title{Full attention with the structural biases added to the logits}
\usage{
graphormer_attention(H, WQ, WK, WV, bias, edge_bias = NULL) 

graphormer(H, WQ, WK, WV, bias, edge_bias = NULL) 
}
\arguments{
\item{H}{Node feature matrix (n x d).}
\item{WQ}{Query projection matrix.}
\item{WK}{Key projection matrix.}
\item{WV}{Value projection matrix.}
\item{bias}{Spatial bias matrix.}
\item{edge_bias}{Optional list of edge biases keyed by (i, j).}
}
\description{
Full attention with the structural biases added to the logits
}
\value{
List with output, weights, method, note.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
spatial_bias(M, V)
graphormer_attention(H = M, WQ = M, WK = M, WV = M, bias = M)
}
