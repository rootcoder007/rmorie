% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.orfgrf_grow_forest}
\alias{.orfgrf_grow_forest}
\title{.orfgrf_grow_forest}
\usage{
.orfgrf_grow_forest(X, y, W = NULL, kind = "double-sample", n_trees = 100, min_leaf = 5, alpha = 0.05, max_depth = 12, seed = 0)
}
\arguments{
\item{X}{Passed to \code{.grow_one_tree}.}
\item{y}{A vector; its length is taken.}
\item{W}{Defaults to \code{NULL}.}
\item{kind}{Defaults to \code{"double-sample"}.}
\item{n_trees}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.}
\item{min_leaf}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{max_depth}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{12}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the orfgrf_native implementation. Called by \code{orthogonal_random_forest}. See the file header for the source the module follows.
}
\value{
A list with \code{trees}, \code{bags}, \code{s}.
}
\keyword{internal}
