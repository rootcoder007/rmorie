% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gnnEx_explain_node}
\alias{gnnEx_explain_node}
\title{gnnEx_explain_node}
\usage{
gnnEx_explain_node(predict, adj, v, y, n_features, L = 2, iters = 300, lr = 0.1, 
    size_coef = 0.005, entropy_coef = 1, seed = 0, penalize = TRUE) 
}
\arguments{
\item{predict}{Passed to \code{gnnEx_mask_objective}.}
\item{adj}{Passed to \code{gnnEx_computation_graph}.}
\item{v}{Coerced to character by the body, with \code{as.character}.}
\item{y}{Passed to \code{gnnEx_mask_objective}.}
\item{n_features}{Coerced to integer by the body, with \code{as.integer}.}
\item{L}{Passed to \code{gnnEx_computation_graph}. Defaults to \code{2}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{300}.}
\item{lr}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{size_coef}{Defaults to \code{0.005}.}
\item{entropy_coef}{Defaults to \code{1}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{penalize}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the gnnEx_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{edges_ranked}, \code{edge_mask}, \code{feature_mask}, \code{loss_history}, \code{final}, \code{computation_graph}, \code{penalized}, \code{method}, \code{note}.
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
