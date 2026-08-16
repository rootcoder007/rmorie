% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{leaf_of}
\alias{leaf_of}
\title{leaf_of}
\usage{
leaf_of(tree, x)
}
\arguments{
\item{tree}{See Usage.}
\item{x}{A vector; indexed elementwise.}
}
\description{
A step of the hntfst_native implementation. Called by \code{.hntfst_tree_predict}, \code{forest_weights}, \code{morie_clrgrf}. See the file header for the source the module follows.
}
\value{
A list with \code{node}, \code{path}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(rnorm(80), 40, 2)
y <- as.numeric(X[, 1] > 0) + rnorm(40, 0, 0.1)
tr <- honest_tree(X, y)
str(leaf_of(tr$tree, X[1, ]), max.level = 1)
}
