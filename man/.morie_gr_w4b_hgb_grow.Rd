% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_w4b_hgb_grow}
\alias{.morie_gr_w4b_hgb_grow}
\title{.morie_gr_w4b_hgb_grow}
\usage{
.morie_gr_w4b_hgb_grow(binned, grad, hess, rows, depth, max_depth, n_bins, min_leaf)
}
\arguments{
\item{binned}{A matrix; indexed by row and column.}
\item{grad}{A vector; indexed elementwise.}
\item{hess}{A vector; indexed elementwise.}
\item{rows}{A vector; its length is taken and its elements indexed.}
\item{depth}{Numeric; combined arithmetically in the body.}
\item{max_depth}{Passed to \code{.morie_gr_w4b_hgb_grow}.}
\item{n_bins}{Passed to \code{.morie_gr_w4b_hgb_best_split}.}
\item{min_leaf}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the geron_w4b_native implementation. Called by \code{morie_geron_histogram_gradient_boosting}. See the file header for the source the module follows.
}
\value{
A list with \code{feature}, \code{bin0}, \code{gain}, \code{left}, \code{right}.
}
\keyword{internal}
