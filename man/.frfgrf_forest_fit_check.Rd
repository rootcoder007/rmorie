% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frfgrf_forest_fit_check}
\alias{.frfgrf_forest_fit_check}
\title{.frfgrf_forest_fit_check}
\usage{
.frfgrf_forest_fit_check(y, X, n_trees = 100, min_leaf = 5, subsample_frac = 0.5, alpha = 0.05, pi = 0.5, seed = 0, kind = "double-sample", sizes = NULL)
}
\arguments{
\item{y}{See Usage.}
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{n_trees}{Defaults to \code{100}.}
\item{min_leaf}{Passed to \code{.frfgrf_honesty_test}. Defaults to \code{5}.}
\item{subsample_frac}{Defaults to \code{0.5}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.05}.}
\item{pi}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{kind}{Passed to \code{.frfgrf_honesty_test}. Defaults to \code{"double-sample"}.}
\item{sizes}{Defaults to \code{NULL}.}
}
\description{
A step of the frfgrf_native implementation. Called by \code{morie_frfgrf}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{passes}, \code{checks}, \code{honesty}, \code{subsample_rate_ok}, \code{subsample_rate_note}, \code{split_share}, \code{split_counts}, \code{random_split_floor}, \code{min_share}, \code{regularity}, \code{alpha}, \code{pi}, \code{beta}, \code{beta_min}, \code{s}, \code{n}, \code{d}, \code{n_trees}, \code{kind}, \code{failed}, \code{method}.
}
\references{
----------
Wager, S. & Athey, S. (2018) "Estimation and Inference of Heterogeneous
Treatment Effects using Random Forests", Journal of the American
Statistical Association 113(523), 1228-1242,
doi:10.1080/01621459.2017.1319839, arXiv:1510.04342. Definitions 2-5,
Theorem 3 and its beta_min, Theorem 1.

Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized Random
Forests", The Annals of Statistics 47(2), 1148-1178,
doi:10.1214/18-AOS1709, arXiv:1610.01271. The same conditions in the
generalized setting.

Biau, G. (2012) "Analysis of a Random Forests Model", Journal of
Machine Learning Research 13, 1063-1095. Earlier consistency analysis
of the same shape.
}
\keyword{internal}
