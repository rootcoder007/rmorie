% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_ecsTCR_co2_forcing}
\alias{morie_ecsTCR_co2_forcing}
\title{Radiative forcing from a CO2 concentration ratio}
\usage{
morie_ecsTCR_co2_forcing(ratio, f2x = .ECSTCR_F2X) 
}
\arguments{
\item{ratio}{CO2 relative to pre-industrial.}
\item{f2x}{forcing from doubling; defaults to AR6's 3.93 W m-2.}
}
\description{
Radiative forcing from a CO2 concentration ratio
}
\value{
the forcing in W m-2.
}
\references{
  Charney, J.G. et al. (1979) "Carbon Dioxide and Climate: A Scientific
    Assessment." National Academy of Sciences, Washington DC.
  Forster, P. et al. (2021) "The Earth's Energy Budget, Climate
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_ecsTCR_co2_forcing(V)
}
