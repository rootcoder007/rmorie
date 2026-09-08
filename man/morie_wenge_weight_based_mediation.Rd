% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_wenge_weight_based_mediation}
\alias{morie_wenge_weight_based_mediation}
\alias{morie_wenge}
\title{morie_wenge_weight_based_mediation}
\usage{
morie_wenge_weight_based_mediation(X, M, C, Y, strategy = "em", saturated = TRUE) 

morie_wenge(X, M, C, Y, strategy = "em", saturated = TRUE) 
}
\arguments{
\item{X}{Passed to \code{.s03vec}.}
\item{M}{Optional; may be \code{NULL}. Passed to \code{morie_wenge_mediation_functional}.}
\item{C}{Optional; may be \code{NULL}. Passed to \code{morie_wenge_mediation_functional}.}
\item{Y}{Passed to \code{.s03vec}.}
\item{strategy}{Passed to \code{morie_wenge_mediation_functional}. Defaults to \code{"em"}.}
\item{saturated}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the wenge_native implementation. No other function in the package calls it. See the file header for the source the module follows.
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
