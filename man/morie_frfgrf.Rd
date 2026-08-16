% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_frfgrf}
\alias{morie_frfgrf}
\title{Main entry point: same argument names and defaults as}
\usage{
morie_frfgrf(y, X, n_trees = 100, min_leaf = 5, subsample_frac = 0.5, alpha = 0.05, pi = 0.5, seed = 0, kind = "double-sample", sizes = NULL)
}
\arguments{
\item{y}{Passed to \code{.frfgrf_forest_fit_check}.}
\item{X}{Passed to \code{.frfgrf_forest_fit_check}.}
\item{n_trees}{Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{100}.}
\item{min_leaf}{Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{5}.}
\item{subsample_frac}{Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{0.5}.}
\item{alpha}{Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{0.05}.}
\item{pi}{Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{0.5}.}
\item{seed}{Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{0}.}
\item{kind}{Passed to \code{.frfgrf_forest_fit_check}. Defaults to \code{"double-sample"}.}
\item{sizes}{Passed to \code{.frfgrf_forest_fit_check}.}
}
\description{
forest_fit_check Part of the frfgrf_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.frfgrf_forest_fit_check}.
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
