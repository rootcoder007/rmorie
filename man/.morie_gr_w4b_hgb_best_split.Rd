% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_w4b_hgb_best_split}
\alias{.morie_gr_w4b_hgb_best_split}
\title{.morie_gr_w4b_hgb_best_split}
\usage{
.morie_gr_w4b_hgb_best_split(binned, grad, hess, n_bins, min_leaf)
}
\arguments{
\item{binned}{A matrix; indexed by row and column.}
\item{grad}{Numeric; passed to \code{sum}.}
\item{hess}{Numeric; passed to \code{sum}.}
\item{n_bins}{Numeric; combined arithmetically in the body.}
\item{min_leaf}{See Usage.}
}
\description{
A step of the geron_w4b_native implementation. Called by \code{.morie_gr_w4b_hgb_grow}. See the file header for the source the module follows.
}
\value{
A list with \code{gain}, \code{feature}, \code{bin0}.
}
\keyword{internal}
