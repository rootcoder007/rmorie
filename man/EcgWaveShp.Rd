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
\item{qrsdur}{Numeric; combined arithmetically in the body.}
\item{stdev}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{rdur}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{sdur}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{qpresent}{Optional; may be \code{NULL}. Coerced to logical by the body, with \code{as.logical}.}
}
\description{
A step of the rangayyan_qrs implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{qrsdurms}, \code{qrswide}, \code{lbbbdur}, \code{rbbbdur}, \code{sdurok}, \code{rdurok}, \code{qabsent}, \code{stdev}, \code{stfinding}, \code{required}, \code{method}.
}
\keyword{internal}
\examples{
EcgWaveShp(qrsdur = 5L, stdev = 5L)
}
