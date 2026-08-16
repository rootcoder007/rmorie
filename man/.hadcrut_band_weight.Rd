% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hadcrut_band_weight}
\alias{.hadcrut_band_weight}
\title{Area weight of a latitude band: the cosine of its centre}
\usage{
.hadcrut_band_weight(i, n_lat)
}
\arguments{
\item{i}{Passed to \code{.hadcrut_cell_lat}.}
\item{n_lat}{Passed to \code{.hadcrut_cell_lat}.}
}
\description{
A step of the hadcrut_native implementation. Called by \code{.hadcrut_region_mean}, \code{morie_hadcrut}. See the file header for the source the module follows.
}
\value{
The value of \code{cos}.
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
.hadcrut_band_weight(V, V)
}
