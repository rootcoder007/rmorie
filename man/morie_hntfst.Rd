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
\item{X}{Passed to \code{honest_forest}.}
\item{y}{Passed to \code{honest_forest}.}
\item{W}{Passed to \code{honest_forest}.}
\item{kind}{Passed to \code{honest_forest}. Defaults to \code{"double-sample"}.}
\item{n_trees}{Passed to \code{honest_forest}. Defaults to \code{200L}.}
\item{subsample_frac}{Passed to \code{honest_forest}. Defaults to \code{0.5}.}
\item{min_leaf}{Passed to \code{honest_forest}. Defaults to \code{5L}.}
\item{alpha}{Passed to \code{honest_forest}. Defaults to \code{0.05}.}
\item{pi}{Passed to \code{honest_forest}. Defaults to \code{0.5}.}
\item{max_depth}{Passed to \code{honest_forest}. Defaults to \code{12L}.}
\item{seed}{Passed to \code{honest_forest}. Defaults to \code{0L}.}
\item{at}{Passed to \code{honest_forest}.}
\item{level}{Passed to \code{honest_forest}. Defaults to \code{0.95}.}
\item{correction}{Passed to \code{honest_forest}. Defaults to \code{TRUE}.}
}
\description{
A step of the hntfst_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{honest_forest}.
}
\keyword{internal}
\examples{
set.seed(3)
X <- matrix(rnorm(120), 60, 2)
y <- X[, 1] + rnorm(60, 0, 0.3)
r <- morie_hntfst(X, y, n_trees = 10L)
str(r, max.level = 1)
}
