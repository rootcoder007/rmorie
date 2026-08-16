% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tree_fit}
\alias{.morie_tree_fit}
\title{Internal helper: fit one tree, returning the tree and its gain vector}
\usage{
.morie_tree_fit(X, g, h, max_depth = 6L, min_node = 1L, mtry = ncol(X), lambda = 0, alpha = 0, gamma_pen = 0)
}
\description{
Delegates to the compiled kernel in src/morie_trees.cpp. The pure-R `.tree_grow` above is the reference implementation and is kept as the fallback for builds without the compiled backend; the two agree exactly, since the kernel applies the same split rule and leaf weight.
}
\value{
A list with \code{tree}, \code{importance}.
}
\keyword{internal}
