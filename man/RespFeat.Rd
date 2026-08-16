% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RespFeat}
\alias{RespFeat}
\title{RespFeat: respiratory rate, depth and I:E ratio}
\usage{
RespFeat(resp, fs, signal_type = "flow", min_breath_s = 1)
}
\arguments{
\item{resp}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{signal_type}{One of \code{"flow"}, \code{"volume"}. Defaults to \code{"flow"}.}
\item{min_breath_s}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{rate_breaths_per_min}, \code{mean_period_s}, \code{sd_period_s}, \code{regularity_cv}, \code{depth}, \code{mean_ti_s}, \code{mean_te_s}, \code{ie_ratio}, \code{breaths}, \code{n_breaths}, \code{signal_type}, \code{fs_hz}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(4)
fs <- 25
tv <- seq(0, 30, by = 1 / fs)
resp <- sin(2 * pi * 0.25 * tv) + 0.05 * rnorm(length(tv))
r <- RespFeat(resp, fs = fs, signal_type = "flow")
str(r, max.level = 1)
}
