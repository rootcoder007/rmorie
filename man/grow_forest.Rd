% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{grow_forest}
\alias{grow_forest}
\title{grow_forest}
\usage{
grow_forest(X, y, W = NULL, kind = "double-sample", n_trees = 200L, subsample_frac = 0.5, min_leaf = 5L, alpha = 0.05, pi = 0.5, max_depth = 12L, seed = 0L, clusters = NULL)
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
\item{clusters}{Defaults to \code{NULL}.}
}
\description{
Part of the hntfst_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{trees}, \code{bags}, \code{s}.
}
\keyword{internal}
