% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_xgb_build}
\alias{.morie_w4d_xgb_build}
\title{.morie_w4d_xgb_build}
\usage{
.morie_w4d_xgb_build(X, g, h, depth, max_depth, lam, gamma, min_child_weight)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{g}{A vector; indexed elementwise.}
\item{h}{A vector; indexed elementwise.}
\item{depth}{Numeric; combined arithmetically in the body.}
\item{max_depth}{Passed to \code{.morie_w4d_xgb_build}.}
\item{lam}{Passed to \code{.morie_w4d_xgb_leaf_weight}.}
\item{gamma}{Passed to \code{.morie_w4d_xgb_gain}.}
\item{min_child_weight}{Passed to \code{.morie_w4d_xgb_build}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_xgboost}. See the file header for the source the module follows.
}
\value{
The value of \code{node}, as built in the body.
}
\keyword{internal}
