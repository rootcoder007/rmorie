% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.grow_one_tree}
\alias{.grow_one_tree}
\title{Minimal honest CART builder. Each tree gets half the observations}
\usage{
.grow_one_tree(X, y, indices, min_leaf, alpha, max_depth, e)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; indexed elementwise.}
\item{indices}{A vector; its length is taken and its elements indexed.}
\item{min_leaf}{See Usage.}
\item{alpha}{See Usage.}
\item{max_depth}{See Usage.}
\item{e}{Passed to \code{.ghc_unif}.}
}
\description{
chosen uniformly without replacement; honesty is enforced by splitting each tree\\'s bag into a structure half and a leaf half.
}
\value{
The value of \code{build}.
}
\keyword{internal}
