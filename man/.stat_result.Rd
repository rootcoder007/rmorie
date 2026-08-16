% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.stat_result}
\alias{.stat_result}
\title{Internal helper: Stat Result}
\usage{
.stat_result(method, test_statistic, p_value, df = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, effect_size = NA_real_, estimate = NA_real_, n = NA_integer_, extra = list())
}
\description{
Internal helper: Stat Result
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.stat_result(method = c(1, 2, 3, 4, 5, 6, 7, 8), test_statistic = c(1, 2, 3, 4, 5, 6, 7, 8), p_value = c(1, 2, 3, 4, 5, 6, 7, 8))
}
