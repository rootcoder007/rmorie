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
