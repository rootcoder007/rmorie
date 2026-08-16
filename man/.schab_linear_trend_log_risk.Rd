% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_linear_trend_log_risk}
\alias{.schab_linear_trend_log_risk}
\title{.schab_linear_trend_log_risk}
\usage{
.schab_linear_trend_log_risk(alpha, u, beta_t, delta_i, times)
}
\arguments{
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{u}{A vector; its length is taken.}
\item{beta_t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{delta_i}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{times}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the schab_glmm_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.schab_linear_trend_log_risk(alpha = 0.5, u = c(1, 2, 3, 4, 5, 6, 7, 8), beta_t = c(1, 2, 3, 4, 5, 6, 7, 8), delta_i = c(0, 1, 0, 1, 1, 0, 1, 0), times = c(1, 2, 3, 4, 5, 6, 7, 8))
}
