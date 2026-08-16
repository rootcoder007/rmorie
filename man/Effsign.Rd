% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Effsign}
\alias{Effsign}
\title{Sign test efficacy -- eq. (13.3.3), p. 489}
\usage{
Effsign(n, fmed)
}
\description{
Sign test efficacy -- eq. (13.3.3), p. 489
}
\value{
A list with \code{efficacy}, \code{per_obs}, \code{n}, \code{fmed}.
}
\keyword{internal}
\examples{
Effsign(n = 5L, fmed = 5L)
}
