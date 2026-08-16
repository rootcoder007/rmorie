% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.center_cate}
\alias{.center_cate}
\title{.center_cate}
\usage{
.center_cate(y, W, X, n_folds, n_trees, min_leaf, seed)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{W}{A vector; indexed elementwise.}
\item{X}{A matrix; indexed by row and column.}
\item{n_folds}{A count; the body uses it as \code{seq_len(...)}.}
\item{n_trees}{Passed to \code{grow_forest}.}
\item{min_leaf}{Passed to \code{grow_forest}.}
\item{seed}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the crfsel_native implementation. Called by \code{morie_crfsel}. See the file header for the source the module follows.
}
\value{
A list with \code{m_hat}, \code{e_hat}.
}
\keyword{internal}
