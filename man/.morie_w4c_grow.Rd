% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_grow}
\alias{.morie_w4c_grow}
\title{.morie_w4c_grow}
\usage{
.morie_w4c_grow(X, y, depth, max_depth, min_leaf, criterion, columns_fn)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{depth}{Numeric; combined arithmetically in the body.}
\item{max_depth}{Passed to \code{.morie_w4c_grow}.}
\item{min_leaf}{Numeric; combined arithmetically in the body.}
\item{criterion}{Passed to \code{.morie_w4c_leaf_value}.}
\item{columns_fn}{Passed to \code{.morie_w4c_grow}.}
}
\description{
A step of the geron_w4c_native implementation. Called by \code{morie_geron_random_forest}, \code{morie_geron_regression_tree}. See the file header for the source the module follows.
}
\value{
A list with \code{leaf}, \code{feature}, \code{threshold}, \code{n}, \code{left}, \code{right}.
}
\keyword{internal}
