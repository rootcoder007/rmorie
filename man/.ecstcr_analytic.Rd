% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ecstcr_analytic}
\alias{.ecstcr_analytic}
\title{One step of the exact solution for a forcing held constant over the}
\usage{
.ecstcr_analytic(T, TD, F, lam, gam, eps, C, CD, h)
}
\arguments{
\item{T}{See Usage.}
\item{TD}{See Usage.}
\item{F}{See Usage.}
\item{lam}{See Usage.}
\item{gam}{See Usage.}
\item{eps}{See Usage.}
\item{C}{See Usage.}
\item{CD}{See Usage.}
\item{h}{See Usage.}
}
\description{
step. The system is linear, so diagonalising the 2x2 matrix gives the answer outright: no tolerance, no step-size sensitivity.
}
\value{
A vector, from \code{c}.
}
\references{
  Charney, J.G. et al. (1979) "Carbon Dioxide and Climate: A Scientific
    Assessment." National Academy of Sciences, Washington DC.
  Forster, P. et al. (2021) "The Earth\\'s Energy Budget, Climate
    Feedbacks, and Climate Sensitivity", Chapter 7 of IPCC AR6 WG1,
    Cambridge University Press, 923-1054,
    doi:10.1017/9781009157896.009
  Gregory, J.M. et al. (2004) "A new method for diagnosing radiative
    forcing and climate sensitivity", Geophysical Research Letters
    31(3), L03205, doi:10.1029/2003GL018747
  Held, I.M. et al. (2010) "Probing the fast and slow components of
    global warming by returning abruptly to preindustrial forcing",
    Journal of Climate 23(9), 2418-2427, doi:10.1175/2009JCLI3466.1
  Geoffroy, O. et al. (2013) "Transient climate response in a two-layer
    energy-balance model. Part I", Journal of Climate 26(6), 1841-1857,
    doi:10.1175/JCLI-D-12-00195.1
}
\keyword{internal}
