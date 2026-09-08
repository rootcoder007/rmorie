% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hadcrut}
\alias{morie_hadcrut}
\title{Blend land and ocean anomaly grids and average them up}
\usage{
morie_hadcrut(T, sst, land_fraction = NULL, sea_ice = NULL, rule = "hadcrut5", 
    route = "hemispheric", interval = "normal", level = 0.95, T_var = NULL, 
    sst_var = NULL, ensemble = NULL, reference = NULL) 
}
\arguments{
\item{T}{Land air temperature anomaly matrix on a regular grid   running south to north, NA where unobserved.}
\item{sst}{Sea-surface temperature anomaly matrix on the same grid.}
\item{land_fraction}{Areal land fraction per cell; all ocean when   omitted.}
\item{sea_ice}{Sea-ice concentration per cell, or NULL. Concentration   at or above the threshold makes the water count as land.}
\item{rule}{A member of the weight-rule list.}
\item{route}{A member of the mean-route list.}
\item{interval}{"normal" builds the interval from the combined   standard error; "ensemble" takes it from the spread of the   ensemble, which is the only route that carries the correlated bias   term properly.}
\item{level}{Interval coverage.}
\item{T_var}{Per-cell land error variance matrix, or NULL.}
\item{sst_var}{Per-cell SST error variance matrix, or NULL.}
\item{ensemble}{A list of realisations of the blended field, or NULL.}
\item{reference}{A list of complete fields for the coverage term, or   NULL.}
}
\description{
Blend land and ocean anomaly grids and average them up
}
\value{
A list with the blended grid, the global and hemispheric   means, the three uncertainty components and the combined interval,   and the coverage actually achieved.
}
\references{
  Morice, C.P., Kennedy, J.J., Rayner, N.A., Winn, J.P., Hogan, E.,
    Killick, R.E., Dunn, R.J.H., Osborn, T.J., Jones, P.D. and
    Simpson, I.R. (2021) "An updated assessment of near-surface
    temperature change from 1850: the HadCRUT5 data set." Journal of
    Geophysical Research: Atmospheres 126(3), e2019JD032361.
    doi:10.1029/2019JD032361.
  Osborn, T.J. et al. (2021) "Land surface air temperature variations
    across the globe updated to 2019: the CRUTEM5 data set." Journal
    of Geophysical Research: Atmospheres 126(2), e2019JD032352.
  Kennedy, J.J., Rayner, N.A., Atkinson, C.P. and Killick, R.E. (2019)
    "An ensemble data set of sea surface temperature change from 1850:
    the Met Office Hadley Centre HadSST.4.0.0.0 data set." Journal of
    Geophysical Research: Atmospheres 124(14), 7719-7763.
  Donlon, C.J. et al. (2012) "The Operational Sea Surface Temperature
    and Sea Ice Analysis (OSTIA) system." Remote Sensing of
    Environment 116, 140-158.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_hadcrut(V, V)
}
