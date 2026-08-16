% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EcgWaveShp}
\alias{EcgWaveShp}
\title{EcgWaveShp}
\usage{
EcgWaveShp(qrsdur, stdev, rdur = NULL, sdur = NULL, qpresent = NULL)
}
\arguments{
\item{qrsdur}{See Usage.}
\item{stdev}{See Usage.}
\item{rdur}{Defaults to \code{NULL}.}
\item{sdur}{Defaults to \code{NULL}.}
\item{qpresent}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_qrs implementation; see the file header for the source it follows.
}
\value{
A list with \code{qrsdurms}, \code{qrswide}, \code{lbbbdur}, \code{rbbbdur}, \code{sdurok}, \code{rdurok}, \code{qabsent}, \code{stdev}, \code{stfinding}, \code{required}, \code{method}.
}
\keyword{internal}
\examples{
EcgWaveShp(qrsdur = 5L, stdev = 5L)
}
