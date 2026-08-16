% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hntfst}
\alias{morie_hntfst}
\title{morie_hntfst}
\usage{
morie_hntfst(X, y, W = NULL, kind = "double-sample", n_trees = 200L, subsample_frac = 0.5, min_leaf = 5L, alpha = 0.05, pi = 0.5, max_depth = 12L, seed = 0L, at = NULL, level = 0.95, correction = TRUE)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{W}{Defaults to \code{NULL}.}
\item{kind}{Defaults to \code{"double-sample"}.}
\item{n_trees}{Defaults to \code{200L}.}
\item{subsample_frac}{Defaults to \code{0.5}.}
\item{min_leaf}{Defaults to \code{5L}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{pi}{Defaults to \code{0.5}.}
\item{max_depth}{Defaults to \code{12L}.}
\item{seed}{Defaults to \code{0L}.}
\item{at}{Defaults to \code{NULL}.}
\item{level}{Defaults to \code{0.95}.}
\item{correction}{Defaults to \code{TRUE}.}
}
\description{
A step of the hntfst_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{honest_forest}.
}
\keyword{internal}
