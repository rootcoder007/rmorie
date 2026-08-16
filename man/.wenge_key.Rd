% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wenge_key}
\alias{.wenge_key}
\title{.wenge_key}
\usage{
.wenge_key(row)
}
\arguments{
\item{row}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the wenge_native implementation. Called by \code{.wenge_saturated_models}, \code{.wenge_saturated_outcome}, \code{morie_wenge_mediation_functional}. See the file header for the source the module follows.
}
\value{
A character value.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.wenge_key(V)
}
