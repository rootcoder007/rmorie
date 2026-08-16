% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sccsno_relative_incidence}
\alias{morie_sccsno_relative_incidence}
\title{Point estimates and Wald intervals on the incidence scale}
\usage{
morie_sccsno_relative_incidence(fit, level = 0.95)
}
\arguments{
\item{fit}{See Usage.}
\item{level}{Defaults to \code{0.95}.}
}
\description{
Part of the sccsno_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{intervals}, \code{level}.
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
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
morie_sccsno_relative_incidence(D)
}
