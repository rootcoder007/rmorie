% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sccsno_band}
\alias{.sccsno_band}
\title{Index of the age band containing t (0-based, matching alpha[j+1])}
\usage{
.sccsno_band(t, age_breaks)
}
\arguments{
\item{t}{See Usage.}
\item{age_breaks}{See Usage.}
}
\description{
A step of the sccsno_native implementation. Called by \code{morie_sccsno_build_intervals}. See the file header for the source the module follows.
}
\value{
The value of \code{j}, as built in the body.
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
.sccsno_band(t = 5L, age_breaks = c(1, 2, 3, 4, 5, 6, 7, 8))
}
