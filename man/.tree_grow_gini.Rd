% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tree_grow_gini}
\alias{.tree_grow_gini}
\title{Internal helper: grow one Gini classification tree}
\usage{
.tree_grow_gini(X, Y, idx, depth, max_depth, min_node, mtry, importance)
}
\description{
Internal helper: grow one Gini classification tree
}
\value{
A list with \code{leaf}, \code{j}, \code{thr}, \code{left}, \code{right}.
}
\keyword{internal}
