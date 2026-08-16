% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_w4b_grid_search}
\alias{.morie_gr_w4b_grid_search}
\title{.morie_gr_w4b_grid_search}
\usage{
.morie_gr_w4b_grid_search(param_grid, X, y, estimator = NULL, K = 3, score = NULL)
}
\arguments{
\item{param_grid}{Passed to \code{names}.}
\item{X}{Passed to \code{.morie_gr_mat}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{estimator}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{K}{Passed to \code{morie_geron_cross_validation_score}. Defaults to \code{3}.}
\item{score}{Passed to \code{morie_geron_cross_validation_score}.}
}
\description{
A step of the geron_w4b_native implementation. Called by \code{morie_geron_hyperparameter_tuning}. See the file header for the source the module follows.
}
\value{
A list with \code{best_params}, \code{best_score}, \code{results}, \code{n_candidates}, \code{n_fits}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
