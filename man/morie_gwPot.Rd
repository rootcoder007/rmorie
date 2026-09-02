% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gwPot}
\alias{morie_gwPot}
\title{morie_gwPot}
\usage{
morie_gwPot(gas, horizon = 100) 
}
\arguments{
\item{gas}{Coerced to character by the body, with \code{as.character}.}
\item{horizon}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.}
}
\description{
Part of the gwPot_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{agwp}, \code{agwp_co2}, \code{gwp_from_agwp}, \code{lifetime}, \code{radiative_efficiency}, \code{gas}, \code{horizon}, \code{method}.
}
\keyword{internal}
\examples{
morie_gwPot("CO2")
}
