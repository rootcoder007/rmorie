% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_ecsTCR_integrate}
\alias{morie_ecsTCR_integrate}
\title{Run the two-layer energy balance model}
\usage{
morie_ecsTCR_integrate(forcing, lam, gamma = 0.7, epsilon = 1, C = 8, C_deep = 100, solver = "analytic", dt = 1, T0 = 0, TD0 = 0)
}
\arguments{
\item{forcing}{radiative forcing, one entry per year.}
\item{lam}{climate feedback parameter, W m-2 K-1.}
\item{gamma}{deep-ocean exchange coefficient.}
\item{epsilon}{efficacy of deep-ocean heat uptake.}
\item{C}{surface heat capacity, W yr m-2 K-1.}
\item{C_deep}{deep-ocean heat capacity.}
\item{solver}{analytic, rk4 or euler.}
\item{dt}{step length in years.}
\item{T0}{initial surface anomaly.}
\item{TD0}{initial deep anomaly.}
}
\description{
Run the two-layer energy balance model
}
\value{
a list with temperature, deep_temperature and imbalance.
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
morie_ecsTCR_integrate(forcing = c(1, 2, 3, 4, 5, 6, 7, 8), lam = 5L)
}
