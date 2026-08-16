% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsSpkiUpd}
\alias{QrsSpkiUpd}
\title{Eq (4.18): SPKI = 0.25 PEAKI + 0.75 SPKI, replacing the 0.125/0.875}
\usage{
QrsSpkiUpd(peaki, spki)
}
\arguments{
\item{peaki}{See Usage.}
\item{spki}{See Usage.}
}
\description{
rule A beat found only by search-back was missed by the primary threshold, so the running estimate is too high and the heavier weight pulls it down.
}
\value{
A list with \code{spki}, \code{previous}, \code{peaki}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsSpkiUpd(V, V)
}
