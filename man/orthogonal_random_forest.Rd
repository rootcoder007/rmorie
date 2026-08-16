% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{orthogonal_random_forest}
\alias{orthogonal_random_forest}
\title{ORF for the heterogeneous treatment effect theta_0(x)}
\usage{
orthogonal_random_forest(Y, T, X, W, x_eval = NULL, n_trees = 100, min_leaf = 5, alpha = 0.05, max_depth = 12, seed = 0, residualize = "local", ridge = 1e-08, kind = "double-sample", leave_one_out = TRUE)
}
\arguments{
\item{Y}{See Usage.}
\item{T}{See Usage.}
\item{X}{See Usage.}
\item{W}{See Usage.}
\item{x_eval}{Defaults to \code{NULL}.}
\item{n_trees}{Defaults to \code{100}.}
\item{min_leaf}{Defaults to \code{5}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{max_depth}{Defaults to \code{12}.}
\item{seed}{Defaults to \code{0}.}
\item{residualize}{Defaults to \code{"local"}.}
\item{ridge}{Defaults to \code{1e-08}.}
\item{kind}{Defaults to \code{"double-sample"}.}
\item{leave_one_out}{Defaults to \code{TRUE}.}
}
\description{
Part of the orfgrf_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{theta}, \code{denominator}, \code{n}, \code{n_trees}, \code{residualize}, \code{n_controls}, \code{n_features}, \code{orthogonal}, \code{method}.
}
\keyword{internal}
