% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_causal_forest_native}
\alias{.morie_causal_forest_native}
\title{Internal helper: native R-learner causal forest}
\usage{
.morie_causal_forest_native(X, y, w, n_trees = 500L, max_depth = 8L, min_node = 10L, subsample = 0.5, n_folds = 5L, random_state = 42L)
}
\description{
Internal helper: native R-learner causal forest
}
\value{
A list with \code{tau}, \code{mu1}, \code{mu0}, \code{ps}, \code{ml_y}.
}
\keyword{internal}
