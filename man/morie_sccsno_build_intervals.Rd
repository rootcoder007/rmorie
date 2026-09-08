% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sccsno_build_intervals}
\alias{morie_sccsno_build_intervals}
\title{morie_sccsno_build_intervals}
\usage{
morie_sccsno_build_intervals(start, end, exposure, event_times, risk_periods, age_breaks) 
}
\arguments{
\item{start}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{end}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{exposure}{Optional; may be \code{NULL}. Passed to \code{.sccsno_cuts}.}
\item{event_times}{See Usage.}
\item{risk_periods}{Passed to \code{.sccsno_rp}.}
\item{age_breaks}{Passed to \code{.sccsno_cuts}.}
}
\description{
A step of the sccsno_native implementation. Called by \code{.smatch_build_intervals}, \code{morie_sccsno_fit}. See the file header for the source the module follows.
}
\value{
The value of \code{cells}, as built in the body.
}
\references{
----------
Farrington, C. P. (1995) "Relative Incidence Estimation from Case
Series for Vaccine Safety Evaluation", Biometrics 51(1), 228-235.
JSTOR stable URL https://www.jstor.org/stable/2533328. Secs. 2-3.

Whitaker, H. J., Farrington, C. P., Spiessens, B. & Musonda, P.
(2006) "Tutorial in biostatistics: The self-controlled case series
method", Statistics in Medicine 25, 1768-1797, doi:10.1002/sim.2302.
}
\keyword{internal}
\examples{
cells <- morie_sccsno_build_intervals(start = 0, end = 365,
                                      exposure = 100,
                                      event_times = c(110, 250),
                                      risk_periods = list(c(0, 30)),
                                      age_breaks = c(180))
cells
}
