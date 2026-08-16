% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ecstcr_deriv}
\alias{.ecstcr_deriv}
\title{.ecstcr_deriv}
\usage{
.ecstcr_deriv(T, TD, F, lam, gam, eps, C, CD)
}
\arguments{
\item{T}{Numeric; combined arithmetically in the body.}
\item{TD}{Numeric; combined arithmetically in the body.}
\item{F}{Numeric; combined arithmetically in the body.}
\item{lam}{Numeric; combined arithmetically in the body.}
\item{gam}{Numeric; combined arithmetically in the body.}
\item{eps}{Numeric; combined arithmetically in the body.}
\item{C}{Numeric; combined arithmetically in the body.}
\item{CD}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the ecsTCR_native implementation. Called by \code{.ecstcr_euler}, \code{.ecstcr_rk4}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\references{
  Charney, J.G. et al. (1979) "Carbon Dioxide and Climate: A Scientific
    Assessment." National Academy of Sciences, Washington DC.
  Forster, P. et al. (2021) "The Earth\'s Energy Budget, Climate
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
