% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hadcrut_blend}
\alias{morie_hadcrut_blend}
\title{Blend a land grid and an SST grid cell by cell}
\usage{
morie_hadcrut_blend(T, sst, land_fraction, sea_ice = NULL, rule = "hadcrut5", T_var = NULL, sst_var = NULL)
}
\arguments{
\item{T}{Land air temperature anomaly matrix, NA where unobserved.}
\item{sst}{Sea-surface temperature anomaly matrix, likewise.}
\item{land_fraction}{Areal land fraction matrix.}
\item{sea_ice}{Sea-ice concentration matrix, or NULL.}
\item{rule}{A member of the weight-rule list.}
\item{T_var}{Per-cell land error variance matrix, or NULL.}
\item{sst_var}{Per-cell SST error variance matrix, or NULL.}
}
\description{
Blend a land grid and an SST grid cell by cell
}
\value{
A list with the blended anomaly matrix, the blended variance   matrix, the land weights and the observation mask.
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
NLAT <- 6L
NLON <- 4L
mk <- function(f) {
    m <- matrix(NA_real_, NLAT, NLON)
    for (i in seq_len(NLAT)) for (j in seq_len(NLON)) m[i, j] <- f(i - 
        1L, j - 1L)
    m
}
LAND <- mk(function(i, j) ((i * 5 + j * 2)%%10)/10)
ICE <- mk(function(i, j) if (i >= 4) (((j * 3)%%4)/4) else 0)
TG <- mk(function(i, j) if ((i * 3 + j)%%5 == 0) NA_real_ else (((i * 
    7 + j * 3)%%11) - 5)/10)
SG <- mk(function(i, j) if ((i + 2 * j)%%4 == 0) NA_real_ else (((i * 
    4 + j * 9)%%13) - 6)/20)
TV <- mk(function(i, j) 0.01 * (1 + (i + j)%%3))
SV <- mk(function(i, j) 0.02 * (1 + (i * j)%%4))
morie_hadcrut_blend(TG, SG, LAND, ICE, "hadcrut5", TV, SV)
}
