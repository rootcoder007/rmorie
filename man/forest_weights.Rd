% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{forest_weights}
\alias{forest_weights}
\title{forest_weights}
\usage{
forest_weights(trees, X, x)
}
\arguments{
\item{trees}{A vector; its length is taken.}
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{x}{See Usage.}
}
\description{
A step of the hntfst_native implementation. Called by \code{.center_cate}, \code{.ipwgrf_forest_nuisances}, \code{.itrgrf_fit_arm} and 5 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
