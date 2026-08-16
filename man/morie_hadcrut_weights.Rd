% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hadcrut_weights}
\alias{morie_hadcrut_weights}
\title{Land and ocean blending weights for one grid cell}
\usage{
morie_hadcrut_weights(land_fraction, sea_ice = 0, has_land = TRUE, has_sst = TRUE, rule = "hadcrut5")
}
\arguments{
\item{land_fraction}{Areal land fraction of the cell.}
\item{sea_ice}{Sea-ice concentration of the cell.}
\item{has_land}{Whether the land data set reports the cell.}
\item{has_sst}{Whether the SST data set reports the cell.}
\item{rule}{A member of the weight-rule list.}
}
\description{
The floor is applied only when the land data set reports the cell. Applying it to an unobserved cell would put weight on a station that does not exist, which is the one thing the rule is not for.
}
\value{
A numeric pair, the land and ocean weights, summing to one   when the cell has any observation and to zero when it has none.
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
