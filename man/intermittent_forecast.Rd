% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{intermittent_forecast}
\alias{intermittent_forecast}
\title{TSB intermittent-demand forecaster}
\usage{
intermittent_forecast(y, method = "tsb", alpha = 0.1, beta = 0.05, horizon = 1L) 
}
\arguments{
\item{y}{Numeric series of non-negative demand.}
\item{method}{Method name; only "tsb" is implemented.}
\item{alpha}{Smoothing for the demand size.}
\item{beta}{Smoothing for the demand probability.}
\item{horizon}{Forecast horizon; only horizon = 1 is used by ADIDA.}
}
\description{
Default base forecaster used by \code{morie_adida}, implementing the Teunter-Syntetos-Babai update so the same numbers come out under the same RNG stream as the Python arm. Only the "tsb" method is implemented here; the default and the one the ADIDA cheatsheet references. Other methods are not ported because they are not needed to mirror the Python arm\'s defaults.
}
\value{
Named list with \code{forecast}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::intermittent_forecast(V)
}
