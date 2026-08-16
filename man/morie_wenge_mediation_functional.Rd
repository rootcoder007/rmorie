% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_wenge_mediation_functional}
\alias{morie_wenge_mediation_functional}
\title{morie_wenge_mediation_functional}
\usage{
morie_wenge_mediation_functional(Y, E, M, X, strategy = "em", saturated = TRUE, ridge = 1e-08)
}
\arguments{
\item{Y}{See Usage.}
\item{E}{See Usage.}
\item{M}{See Usage.}
\item{X}{See Usage.}
\item{strategy}{Defaults to \code{"em"}.}
\item{saturated}{Defaults to \code{TRUE}.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
Part of the wenge_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\references{
----------
Tchetgen Tchetgen, E. J. & Shpitser, I. (2012) "Semiparametric theory
for causal mediation analysis", The Annals of Statistics 40(3),
1816-1845, doi:10.1214/12-AOS990; arXiv:1210.4654. Equation (2), Sec.
3.

Imai, K., Keele, L. & Tingley, D. (2010) "A general approach to
causal mediation analysis", Psychological Methods 15(4), 309-334,
doi:10.1037/a0020761.

Pearl, J. (2001) "Direct and indirect effects", UAI 17, 411-420.
}
\keyword{internal}
