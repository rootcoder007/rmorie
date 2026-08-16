% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sccsno_cuts}
\alias{.sccsno_cuts}
\title{Ordered distinct cutpoints for one individual (Fig. 1)}
\usage{
.sccsno_cuts(start, end, exposure, risk_periods, age_breaks)
}
\arguments{
\item{start}{See Usage.}
\item{end}{See Usage.}
\item{exposure}{See Usage.}
\item{risk_periods}{See Usage.}
\item{age_breaks}{See Usage.}
}
\description{
Part of the sccsno_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{sort}.
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
