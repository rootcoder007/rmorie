% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_did_result}
\alias{.morie_did_result}
\title{.morie_did_result}
\usage{
.morie_did_result(estimate, std_error, n_treated, n_control, method, alpha = 0.05, details = list())
}
\description{
.morie_did_result
}
\value{
A list with \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value}, \code{ci_lower}, \code{ci_upper}, \code{n_treated}, \code{n_control}, \code{method}, \code{details}.
}
\keyword{internal}
\examples{
.morie_did_result(estimate = 5L, std_error = 5L, n_treated = c(1, 2, 3, 4, 5, 6, 7, 8), n_control = c(1, 2, 3, 4, 5, 6, 7, 8), method = c(1, 2, 3, 4, 5, 6, 7, 8))
}
