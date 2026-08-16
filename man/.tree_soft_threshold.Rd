% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tree_soft_threshold}
\alias{.tree_soft_threshold}
\title{Soft-thresholding operator for the optional L1 term on leaf weights}
\usage{
.tree_soft_threshold(g, alpha)
}
\arguments{
\item{g}{Numeric; combined arithmetically in the body.}
\item{alpha}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the trees_native implementation. Called by \code{.tree_leaf_weight}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
