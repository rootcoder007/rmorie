% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tree_grow}
\alias{.tree_grow}
\title{Internal helper: grow one regression / second-order tree}
\usage{
.tree_grow(X, g, h, idx, depth, max_depth, min_node, mtry, lambda, alpha, gamma_pen, importance)
}
\description{
`g` and `h` are the first and second derivatives of the loss at the current predictions. Squared-error loss gives g = f - y and h = 1, in which case variance reduction and the second-order gain coincide.
}
\value{
A list with \code{leaf}, \code{j}, \code{thr}, \code{left}, \code{right}.
}
\keyword{internal}
