% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.orfgrf_leaf_value}
\alias{.orfgrf_leaf_value}
\title{Minimal honest CART builder. Each tree gets half the observations}
\usage{
.orfgrf_leaf_value(y, leaf, idxs)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{Outcome vector.}
\item{indices}{A vector; its length is taken and its elements indexed.}
\item{min_leaf}{Accepted by the signature and not used anywhere in the body.}
\item{alpha}{Accepted by the signature and not used anywhere in the body.}
\item{max_depth}{Accepted by the signature and not used anywhere in the body.}
\item{e}{Passed to \code{.ghc_unif}.}
\item{leaf}{Row indices of the leaf half.}
\item{idxs}{Row indices in this node.}
}
\description{
chosen uniformly without replacement; honesty is enforced by splitting each tree\\'s bag into a structure half and a leaf half.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.orfgrf_leaf_value(y = c(1, 2, 3, 4, 5, 6, 7, 8), leaf = c(1, 2, 3, 4, 5, 6, 7, 8), idxs = c(1, 2, 3, 4, 5, 6, 7, 8))
}
