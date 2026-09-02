% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_gsageemd_embed}
\alias{morie_gsageemd_embed}
\alias{graphsage}
\title{GraphSAGE: embeddings for nodes the model has never seen}
\usage{
morie_gsageemd_embed(features, adj, Ws, how = "mean", sizes = NULL, seed = 0) 

graphsage(features, adj, Ws, how = "mean", sizes = NULL, seed = 0) 
}
\description{
GraphSAGE: embeddings for nodes the model has never seen. Inductive Representation Learning on Large Graphs, NeurIPS 2017 (arXiv:1706.02216) -- Algorithm 1, the three aggregators and Supervised Classification with Graph Convolutional Networks, ICLR 2017 (arXiv:1609.02907) -- the transductive convolution that GraphSAGE extends. Native implementation mirroring Python morie.fn.gsageemd exactly: the same mean, max_pool and lstm_order aggregators, the same fixed-size neighbour sampling with replacement when the budget exceeds the neighbourhood, the same L2 normalisation after each layer, and the same unsupervised graph-based loss.
}
\arguments{
\item{features}{Argument \code{features}; see Usage.}
\item{adj}{Argument \code{adj}; see Usage.}
\item{Ws}{Argument \code{Ws}; see Usage.}
\item{how}{Argument \code{how}; see Usage.}
\item{sizes}{Argument \code{sizes}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
}
\value{
A list with components \code{estimate}, \code{embeddings}, \code{depth}, \code{aggregator}, \code{per_batch_bound}, \code{method}, \code{note}.
}
\references{
Sources: Hamilton, W. L., Ying, R. and Leskovec, J. (2017),
the unsupervised loss; Kipf, T. N. and Welling, M. (2017), Semi-
}
\keyword{internal}
\examples{
adj <- list(`0` = c(`1` = 1, `3` = 1), `1` = c(`0` = 1, `2` = 1),
            `2` = c(`1` = 1, `3` = 1), `3` = c(`2` = 1, `0` = 1))
feats <- matrix(runif(12), 4, 3)
W1 <- matrix(rnorm(6 * 4, 0, 0.3), 4, 6)
W2 <- matrix(rnorm(8 * 2, 0, 0.3), 2, 8)
emb <- morie_gsageemd_embed(feats, adj, list(W1, W2), how = "mean", seed = 1)
dim(emb)
}
