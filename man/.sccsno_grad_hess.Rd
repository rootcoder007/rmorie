% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sccsno_grad_hess}
\alias{.sccsno_grad_hess}
\title{.sccsno_grad_hess}
\usage{
.sccsno_grad_hess(params, cells_by_person, n_risk, n_age)
}
\arguments{
\item{params}{See Usage.}
\item{cells_by_person}{See Usage.}
\item{n_risk}{See Usage.}
\item{n_age}{See Usage.}
}
\description{
Part of the sccsno_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{g}, \code{H}.
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
