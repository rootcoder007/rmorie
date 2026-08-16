% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_random_forest}
\alias{morie_geron_random_forest}
\title{Random forest: bagged CART trees with random per-split feature subsets (Geron Ch 6, hmrfc)}
\usage{
morie_geron_random_forest(X, y, n_estimators = 10, max_features = "sqrt", seed = 0, max_depth = 4, min_samples_leaf = 1, task = "auto")
}
\arguments{
\item{X,y}{Data and targets.}
\item{n_estimators,max_features,seed,max_depth,min_samples_leaf,task}{Controls.}
}
\description{
Random forest: bagged CART trees with random per-split feature subsets (Geron Ch 6, hmrfc)
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_random_forest(V, V)
}
