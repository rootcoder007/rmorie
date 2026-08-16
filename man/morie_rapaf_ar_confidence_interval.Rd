% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rapaf_ar_confidence_interval}
\alias{morie_rapaf_ar_confidence_interval}
\title{Monte Carlo confidence interval for AR}
\usage{
morie_rapaf_ar_confidence_interval(case_counts, rate_ratios, log_rr_se, level = 0.95, draws = 2000L, seed = 0L)
}
\arguments{
\item{case_counts}{Non-negative case counts per stratum.}
\item{rate_ratios}{Positive rate ratios.}
\item{log_rr_se}{Standard errors of the log rate ratios.}
\item{level}{Confidence level in \code{(0, 1)}.}
\item{draws}{Number of Monte Carlo draws.}
\item{seed}{Seed for the shared generator.}
}
\description{
Resamples the log rate ratios, recomputes the AR, and reports the percentiles. A delta-method interval on the AR scale can cross 1, which is impossible; this cannot.
}
\value{
A list with \code{estimate}, \code{lower}, \code{upper},   \code{level}, \code{draws}.
}
\keyword{internal}
\examples{
morie_rapaf_ar_confidence_interval(case_counts = c(1, 2, 3, 4, 5, 6, 7, 8), rate_ratios = c(1, 2, 3, 4, 5, 6, 7, 8), log_rr_se = c(1, 2, 3, 4, 5, 6, 7, 8))
}
