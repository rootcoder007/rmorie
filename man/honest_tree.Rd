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
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{W}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{kind}{One of \code{"double-sample"}, \code{"propensity"}. Defaults to \code{"double-sample"}.}
\item{min_leaf}{Numeric; combined arithmetically in the body. Defaults to \code{5L}.}
\item{alpha}{Passed to \code{._best_split}. Defaults to \code{0.05}.}
\item{pi}{Numeric; passed to \code{max}. Defaults to \code{0.5}.}
\item{max_depth}{Passed to \code{>=}. Defaults to \code{12L}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0L}.}
\item{subsample}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the hntfst_native implementation. Called by \code{.frfgrf_honesty_test}, \code{grow_forest}, \code{honest_forest}. See the file header for the source the module follows.
}
\value{
A list with \code{tree}, \code{info}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(rnorm(80), 40, 2)
y <- as.numeric(X[, 1] > 0) + rnorm(40, 0, 0.1)
tr <- honest_tree(X, y)
str(tr, max.level = 1)
}
