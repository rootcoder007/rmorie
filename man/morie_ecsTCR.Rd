% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_ecsTCR}
\alias{morie_ecsTCR}
\title{Equilibrium and transient climate sensitivity}
\usage{
morie_ecsTCR(model_run = NULL, CO2_traj = NULL, route = "parameters", lam = NULL, gamma = 0.7, epsilon = 1, C = 8, C_deep = 100, f2x = .ECSTCR_F2X, solver = "analytic", years = 70L, rate = 0.01, temperature = NULL, imbalance = NULL, forcing_multiple = 2, dt = 1)
}
\arguments{
\item{model_run}{a surface temperature series; alias for temperature.}
\item{CO2_traj}{CO2 relative to pre-industrial, one entry per year.   When absent the 1 percent per year trajectory is built from rate   and years.}
\item{route}{parameters, gregory or emulate.}
\item{lam}{climate feedback parameter; required for the parameters   route, fitted by the others.}
\item{gamma}{deep-ocean exchange coefficient.}
\item{epsilon}{efficacy of deep-ocean heat uptake.}
\item{C}{surface heat capacity.}
\item{C_deep}{deep-ocean heat capacity.}
\item{f2x}{forcing from doubling CO2; defaults to AR6's 3.93 W m-2.}
\item{solver}{analytic, rk4 or euler.}
\item{years}{length of the 1 percent per year run; 70 is the doubling   year.}
\item{rate}{annual fractional CO2 increase.}
\item{temperature}{surface temperature series, for the fitting routes.}
\item{imbalance}{net top-of-atmosphere imbalance, for the Gregory   route.}
\item{forcing_multiple}{what the step experiment did: 2 for   abrupt-2x, 4 for abrupt-4x.}
\item{dt}{step length in years.}
}
\description{
Equilibrium and transient climate sensitivity
}
\value{
a list with ecs, tcr, tcr_ecs_ratio, lambda, f2x,   doubling_year, realised_warming_fraction, temperature,   deep_temperature, imbalance, fitted, charney_range, within_charney,   route, solver and method.
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
