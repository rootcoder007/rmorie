% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{honest_forest}
\alias{honest_forest}
\title{honest_forest}
\usage{
honest_forest(X, y, W = NULL, kind = "double-sample", n_trees = 200L, subsample_frac = 0.5, min_leaf = 5L, alpha = 0.05, pi = 0.5, max_depth = 12L, seed = 0L, at = NULL, level = 0.95, correction = TRUE)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{W}{Defaults to \code{NULL}.}
\item{kind}{Compared against \code{"adaptive"}. Defaults to \code{"double-sample"}.}
\item{n_trees}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{subsample_frac}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{min_leaf}{Numeric; combined arithmetically in the body. Defaults to \code{5L}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{pi}{Defaults to \code{0.5}.}
\item{max_depth}{Defaults to \code{12L}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0L}.}
\item{at}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
\item{correction}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the hntfst_native implementation. Called by \code{morie_hntfst}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{fitted}, \code{se}, \code{ci}, \code{variance}, \code{n}, \code{s}, \code{n_trees}, \code{split_counts}, \code{split_share}, \code{mean_depth}, \code{kind}, \code{honest}, \code{correction}, \code{level}, \code{method}.
}
\keyword{internal}
