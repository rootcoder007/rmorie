% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sccsno_risk}
\alias{.sccsno_risk}
\title{Index of the risk period containing t; 0 is the control period}
\usage{
.sccsno_risk(t, exposure, risk_periods)
}
\arguments{
\item{t}{See Usage.}
\item{exposure}{See Usage.}
\item{risk_periods}{A matrix; indexed by row and column.}
}
\description{
A step of the sccsno_native implementation. Called by \code{morie_sccsno_build_intervals}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
