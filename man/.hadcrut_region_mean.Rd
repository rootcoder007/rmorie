% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hadcrut_region_mean}
\alias{.hadcrut_region_mean}
\title{Cosine-weighted mean over the given rows, and its variance. The mean}
\usage{
.hadcrut_region_mean(grid, rows, var = NULL)
}
\arguments{
\item{grid}{A matrix; indexed by row and column.}
\item{rows}{See Usage.}
\item{var}{Optional; may be \code{NULL}. A matrix; indexed by row and column.}
}
\description{
is NA when the region holds no observation, which is a real state in 1850 and must not silently become zero.
}
\value{
A list with \code{mean}, \code{var}, \code{weight}, \code{n}.
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
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.hadcrut_region_mean(D, V)
}
