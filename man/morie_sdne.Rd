% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sdne}
\alias{morie_sdne}
\title{morie_sdne}
\usage{
morie_sdne(adjacency, reconstruction, embeddings, beta = 5, alpha = 0.1, nu = 0, parameters = NULL)
}
\arguments{
\item{adjacency}{See Usage.}
\item{reconstruction}{See Usage.}
\item{embeddings}{See Usage.}
\item{beta}{Defaults to \code{5}.}
\item{alpha}{Defaults to \code{0.1}.}
\item{nu}{Defaults to \code{0}.}
\item{parameters}{Defaults to \code{NULL}.}
}
\description{
Part of the sdne_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{loss}, \code{second_order}, \code{first_order}, \code{regulariser}, \code{alpha}, \code{beta}, \code{method}, \code{note}.
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
morie_sdne(adjacency = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), reconstruction = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), embeddings = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
