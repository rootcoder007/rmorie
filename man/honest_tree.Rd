% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{honest_tree}
\alias{honest_tree}
\title{honest_tree}
\usage{
honest_tree(X, y, W = NULL, kind = "double-sample", min_leaf = 5L, alpha = 0.05, pi = 0.5, max_depth = 12L, seed = 0L, subsample = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{W}{Defaults to \code{NULL}.}
\item{kind}{Defaults to \code{"double-sample"}.}
\item{min_leaf}{Defaults to \code{5L}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{pi}{Defaults to \code{0.5}.}
\item{max_depth}{Defaults to \code{12L}.}
\item{seed}{Defaults to \code{0L}.}
\item{subsample}{Defaults to \code{NULL}.}
}
\description{
Part of the hntfst_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{tree}, \code{info}.
}
\keyword{internal}
