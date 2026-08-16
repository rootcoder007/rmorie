% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gnnEx_sig}
\alias{.gnnEx_sig}
\title{Vectorised sigmoid with overflow protection (matches Python _sig)}
\usage{
.gnnEx_sig(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
Part of the gnnEx_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
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
