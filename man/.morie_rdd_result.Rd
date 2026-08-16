% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rdd_result}
\alias{.morie_rdd_result}
\title{Internal helper: Morie Rdd Result}
\usage{
.morie_rdd_result(estimate, se, n, method, alpha = 0.05, details = list())
}
\description{
Internal helper: Morie Rdd Result
}
\value{
A list with \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_obs}, \code{method}, \code{details}.
}
\keyword{internal}
\examples{
.morie_rdd_result(estimate = c(1, 2, 3, 4, 5, 6, 7, 8), se = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, method = c(1, 2, 3, 4, 5, 6, 7, 8))
}
