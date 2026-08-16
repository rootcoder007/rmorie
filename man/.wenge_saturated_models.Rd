% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.wenge_saturated_models}
\alias{.wenge_saturated_models}
\title{Nonparametric cell estimates of f(E|X) and f(M|E,X)}
\usage{
.wenge_saturated_models(ev, Mm, Xm)
}
\arguments{
\item{ev}{A vector; its length is taken and its elements indexed.}
\item{Mm}{A matrix; indexed by row and column.}
\item{Xm}{A matrix; indexed by row and column.}
}
\description{
A step of the wenge_native implementation. Called by \code{morie_wenge_mediation_functional}, \code{morie_wenge_weight_based_mediation}. See the file header for the source the module follows.
}
\value{
A list with \code{fe1}, \code{fm}.
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
.wenge_saturated_models(ev = 5L, Mm = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), Xm = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
