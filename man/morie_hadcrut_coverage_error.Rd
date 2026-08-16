% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_hadcrut_coverage_error}
\alias{morie_hadcrut_coverage_error}
\title{The coverage error of one complete field under one coverage mask}
\usage{
morie_hadcrut_coverage_error(reference, seen, route = "hemispheric")
}
\arguments{
\item{reference}{A complete anomaly matrix.}
\item{seen}{The observation mask.}
\item{route}{A member of the mean-route list.}
}
\description{
The complete field is averaged twice: over everything, and over only the cells the observations actually reach. The difference is one realisation of the coverage error -- not a bound on it, a draw from it -- and the root mean square over several reference fields is the coverage uncertainty. This is the only honest way to get the term: it cannot be derived from the observations, because the observations are precisely what is missing where it matters.
}
\value{
The signed coverage error, or NA when either average is   undefined.
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
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_hadcrut_coverage_error(M, M)
}
