% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_adida}
\alias{morie_adida}
\title{ADIDA forecast: aggregate, forecast, disaggregate}
\usage{
morie_adida(y, m, horizon = 1L, method = "tsb", alpha = 0.1, beta = 0.05, overlapping = FALSE, profile = NULL, lead_time = NULL)
}
\arguments{
\item{y}{Numeric demand series.}
\item{m}{Bucket size, positive integer.}
\item{horizon}{Forecast horizon in periods.}
\item{method}{Base forecaster method name (default "tsb").}
\item{alpha}{Size smoothing for the TSB base.}
\item{beta}{Probability smoothing for the TSB base.}
\item{overlapping}{If TRUE, use a rolling bucket window.}
\item{profile}{Optional disaggregation weights, length \code{m}.}
\item{lead_time}{If supplied, overrides \code{m} with the lead time.}
}
\description{
\code{lead_time} sets \code{m} to the lead time, the paper\'s recommendation: the aggregated forecast is then the lead-time demand directly, and no disaggregation error is incurred at all. The disaggregation-sums-back identity is checked exactly so a profile that did not reconstitute would surface as an inconsistency rather than silently change the total.
}
\value{
Named list mirroring the Python payload: \code{estimate},   \code{forecast}, \code{aggregate_forecast}, \code{lead_time_demand},   \code{aggregated}, \code{m}, \code{zero_fraction_original},   \code{zero_fraction_aggregated}, \code{n_buckets}, \code{overlapping},   \code{base_method}, \code{disaggregation_sums_back}, \code{method}.
}
\keyword{internal}
\examples{
morie_adida(y = c(1, 2, 3, 4, 5, 6, 7, 8), m = 3L)
}
