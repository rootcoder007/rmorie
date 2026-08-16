% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tree_best_split}
\alias{.tree_best_split}
\title{Internal helper: best split of one node under the second-order objective}
\usage{
.tree_best_split(X, g, h, idx, feats, min_node, lambda, gamma_pen)
}
\description{
Returns NULL when no split improves the objective. `feats` is the set of columns this node is allowed to consider, which is how mtry enters.
}
\value{
The value of \code{if}.
}
\keyword{internal}
