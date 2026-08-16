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
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{W}{A matrix; passed to \code{as.matrix}.}
\item{x_eval}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{n_trees}{Defaults to \code{100}.}
\item{min_leaf}{Defaults to \code{5}.}
\item{alpha}{Defaults to \code{0.05}.}
\item{max_depth}{Defaults to \code{12}.}
\item{seed}{Defaults to \code{0}.}
\item{residualize}{Defaults to \code{"local"}.}
\item{ridge}{Defaults to \code{1e-08}.}
\item{kind}{Passed to \code{.orfgrf_grow_forest}. Defaults to \code{"double-sample"}.}
\item{leave_one_out}{Defaults to \code{TRUE}.}
}
\description{
A step of the orfgrf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{theta}, \code{denominator}, \code{n}, \code{n_trees}, \code{residualize}, \code{n_controls}, \code{n_features}, \code{orthogonal}, \code{method}.
}
\keyword{internal}
\examples{
orthogonal_random_forest(Y = c(1, 2, 3, 4, 5, 6, 7, 8), T = c(2.5, 1.0, 3.5, 4.0, 2.0, 5.5, 3.0, 6.5), X = c(1, 2, 3, 4, 5, 6, 7, 8), W = c(1, 2, 3, 4, 5, 6, 7, 8))
}
