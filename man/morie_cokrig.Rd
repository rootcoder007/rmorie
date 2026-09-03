% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_cokrig}
\alias{morie_cokrig}
\title{Maximum-likelihood spatial models, point-pattern K, co-kriging and}
\usage{
morie_cokrig(coords, z1, z2, s0, cross_vario = NULL, model = NULL) 
}
\description{
Maximum-likelihood spatial models, point-pattern K, co-kriging and the local differential privacy mechanism. R mirror of the corresponding block in morie/src/morie/fn/_robust_core.py.  The two spatial regressions are checked against spatialreg::lagsarlm and spatialreg::errorsarlm -- note those live in *spatialreg*, not *spdep*, since spdep was split. ' @noRd
}
\arguments{
\item{coords}{Argument \code{coords}; see Usage.}
\item{z1}{Argument \code{z1}; see Usage.}
\item{z2}{Argument \code{z2}; see Usage.}
\item{s0}{Argument \code{s0}; see Usage.}
\item{cross_vario}{Argument \code{cross_vario}; see Usage.}
\item{model}{Argument \code{model}; see Usage.}
}
\value{
A list with components \code{prediction}, \code{variance}, \code{lambda}, \code{mu}, \code{n}, \code{range}.
}
\keyword{internal}
\examples{
morie_cokrig(coords = c(1, 2, 3, 4, 5, 6, 7, 8), z1 = c(1, 2, 3, 4, 5, 6, 7, 8),
  z2 = c(1, 2, 3, 4, 5, 6, 7, 8), s0 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
