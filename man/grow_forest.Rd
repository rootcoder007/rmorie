% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{grow_forest}
\alias{grow_forest}
\title{grow_forest}
\usage{
grow_forest(X, y, W = NULL, kind = "double-sample", n_trees = 200L, subsample_frac = 0.5, 
    min_leaf = 5L, alpha = 0.05, pi = 0.5, max_depth = 12L, seed = 0L, 
    clusters = NULL) 
}
\arguments{
\item{X}{Passed to \code{honest_tree}.}
\item{y}{A vector; its length is taken.}
\item{W}{Passed to \code{honest_tree}.}
\item{kind}{Passed to \code{honest_tree}. Defaults to \code{"double-sample"}.}
\item{n_trees}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{subsample_frac}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{min_leaf}{Numeric; combined arithmetically in the body. Defaults to \code{5L}.}
\item{alpha}{Passed to \code{honest_tree}. Defaults to \code{0.05}.}
\item{pi}{Passed to \code{honest_tree}. Defaults to \code{0.5}.}
\item{max_depth}{Passed to \code{honest_tree}. Defaults to \code{12L}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0L}.}
\item{clusters}{Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.}
}
\description{
A step of the hntfst_native implementation. Called by \code{.center_cate}, \code{.frfgrf_forest_fit_check}, \code{.ipwgrf_forest_nuisances} and 6 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{trees}, \code{bags}, \code{s}.
}
\keyword{internal}
\examples{
set.seed(2)
X <- matrix(rnorm(120), 60, 2)
y <- X[, 1] + rnorm(60, 0, 0.3)
f <- grow_forest(X, y, n_trees = 10L)
length(f$trees)
}
