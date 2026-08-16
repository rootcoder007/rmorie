% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_iv_result}
\alias{.morie_iv_result}
\title{.morie_iv_result}
\usage{
.morie_iv_result(coef_vec, se_vec, n_obs, method, alpha = 0.05, dof = NA, details = list())
}
\description{
.morie_iv_result
}
\value{
A list with \code{coefficients}, \code{std_errors}, \code{t_stats}, \code{p_values}, \code{ci_lower}, \code{ci_upper}, \code{variable_names}, \code{n_obs}, \code{method}, \code{details}.
}
\keyword{internal}
\examples{
.morie_iv_result(coef_vec = c(1, 2, 3, 4, 5, 6, 7, 8), se_vec = c(1, 2, 3, 4, 5, 6, 7, 8), n_obs = c(1, 2, 3, 4, 5, 6, 7, 8), method = c(1, 2, 3, 4, 5, 6, 7, 8))
}
