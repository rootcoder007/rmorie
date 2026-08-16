% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sdne_second_order_loss}
\alias{.sdne_second_order_loss}
\title{.sdne_second_order_loss}
\usage{
.sdne_second_order_loss(adjacency, reconstruction, beta = 5)
}
\arguments{
\item{adjacency}{Passed to \code{.sdne_mat}.}
\item{reconstruction}{Passed to \code{.sdne_mat}.}
\item{beta}{Passed to \code{.sdne_penalty_matrix}. Defaults to \code{5}.}
}
\description{
A step of the sdne_native implementation. Called by \code{morie_sdne}. See the file header for the source the module follows.
}
\value{
A list with \code{loss}, \code{unweighted}, \code{beta}, \code{note}.
}
\references{
----------
Wang, D., Cui, P. & Zhu, W. (2016) "Structural Deep Network
Embedding", Proceedings of the 22nd ACM SIGKDD International
Conference on Knowledge Discovery and Data Mining (KDD \'16),
1225-1234, doi:10.1145/2939672.2939753. Sec. 1 and 3: that shallow
models such as IsoMap, Laplacian Eigenmaps and LINE have limited
representation ability and cannot capture the highly non-linear
network structure; the semi-supervised deep model exploiting
first-order and second-order proximity JOINTLY, with the unsupervised
component reconstructing the second-order proximity to preserve the
GLOBAL structure and the supervised component using first-order
proximity to preserve the LOCAL structure; that first-order proximity
is the local pairwise similarity only between linked vertices and is
insufficient because network sparsity means many legitimate links are
missing; and the re-weighted reconstruction imposing more penalty on
the reconstruction error of non-zero elements than on zero elements.

Belkin, M. & Niyogi, P. (2003) "Laplacian Eigenmaps for
Dimensionality Reduction and Data Representation", Neural
Computation 15(6), 1373-1396, doi:10.1162/089976603321780317. The
first-order term.

Tang, J., Qu, M., Wang, M., Zhang, M., Yan, J. & Mei, Q. (2015)
"LINE: Large-scale Information Network Embedding", WWW 2015,
1067-1077, doi:10.1145/2736277.2741093, arXiv:1503.03578. The
shallow model whose two proximities this deepens.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sdne_second_order_loss(V, V)
}
