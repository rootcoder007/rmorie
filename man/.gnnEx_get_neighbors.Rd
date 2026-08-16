% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gnnEx_get_neighbors}
\alias{.gnnEx_get_neighbors}
\title{Adj is a list: names are node IDs (as character or integer), values}
\usage{
.gnnEx_get_neighbors(adj, u)
}
\arguments{
\item{adj}{See Usage.}
\item{u}{See Usage.}
}
\description{
are integer vectors of neighbours. Falls back to 1-based integer indexing when the list has no names.
}
\value{
A vector, from \code{integer}.
}
\references{
----------
Ying, R., Bourgeois, D., You, J., Zitnik, M. & Leskovec, J. (2019)
"GNNExplainer: Generating Explanations for Graph Neural Networks",
Advances in Neural Information Processing Systems 32 (NeurIPS 2019),
9240-9251, arXiv:1903.03894. Sec. 4.1 (the MI objective, H(Y) being
constant for a trained GNN so the problem reduces to minimising
conditional entropy, and the mean-field variational approximation
learning a real-valued graph mask). Sec. 4.2 (the feature mask).

Kipf, T. N. & Welling, M. (2017) "Semi-Supervised Classification with
Graph Convolutional Networks", ICLR 2017, arXiv:1609.02907. The model
class being explained.
}
\keyword{internal}
