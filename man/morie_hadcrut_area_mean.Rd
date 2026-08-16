% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hadcrut_area_mean}
\alias{morie_hadcrut_area_mean}
\title{Average a grid up to a global figure}
\usage{
morie_hadcrut_area_mean(grid, route = "hemispheric", var = NULL)
}
\arguments{
\item{grid}{Anomaly matrix, NA where unobserved.}
\item{route}{A member of the mean-route list.}
\item{var}{Per-cell variance matrix, or NULL.}
}
\description{
"area" is one cosine-weighted mean over every observed cell; "hemispheric" is the mean of the two hemispheric means, which is HadCRUT5's headline convention and stops the better-observed hemisphere from carrying the global number; "land_ratio" is the land-record convention, two parts north to one part south, in the ratio of the hemispheres' land areas.
}
\value{
A list with the mean, its variance, the total weight, the   cell counts and the two hemispheric means.
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
