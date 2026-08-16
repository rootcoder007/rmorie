% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grow_one_tree}
\alias{.grow_one_tree}
\title{Internal: one honest tree}
\usage{
.grow_one_tree(X, y, indices, min_leaf, alpha, max_depth, e, split_target = NULL)
}
\arguments{
\item{X}{Numeric matrix of features.}
\item{y}{Outcome, used for leaf values.}
\item{indices}{Rows available to this tree.}
\item{min_leaf}{Minimum rows per side, in both halves.}
\item{alpha}{Imbalance floor: a side must keep at least this   fraction of the node\'s split-half rows.}
\item{max_depth}{Depth limit.}
\item{e}{The shared random stream.}
\item{split_target}{What the splits are scored on: the treatment for   a propensity tree, the outcome otherwise.}
}
\description{
Honesty means the rows that CHOOSE a split are not the rows that value the leaf. That requires carrying both halves down the recursion and partitioning each by the chosen threshold: the split half J scores candidate cuts, the estimation half I supplies the leaf value, and a cut is only taken if BOTH halves keep at least \code{min_leaf} rows on each side. The previous version carried only the split half down and left the estimation half as a fixed set, so \code{leaf[leaf \%in\% idxs]} was empty at every node -- the two halves are disjoint by construction -- and every candidate scored \code{NaN}. No split was ever taken and every tree was a single leaf.
}
\value{
A nested list of nodes, each either a leaf with a   \code{value} or a split with \code{j}, \code{cut}, \code{left} and   \code{right}.
}
\keyword{internal}
