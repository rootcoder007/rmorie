% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsThresh}
\alias{QrsThresh}
\title{Eqs (4.16) and (4.17).  The 0.125/0.875 split is a long-memory}
\usage{
QrsThresh(peaki, spki, npki, issignal)
}
\arguments{
\item{peaki}{See Usage.}
\item{spki}{See Usage.}
\item{npki}{See Usage.}
\item{issignal}{See Usage.}
}
\description{
recursive average, so the estimates track slow drift without being thrown by one artifact.  I1 sits a quarter of the way from noise up to signal; I2 is half of I1 and is reserved for search-back.
}
\value{
A list with \code{spki}, \code{npki}, \code{thresh1}, \code{thresh2}, \code{peaki}, \code{issignal}, \code{method}.
}
\keyword{internal}
\examples{
QrsThresh(peaki = c(1, 2, 3, 4, 5, 6, 7, 8), spki = c(1, 2, 3, 4, 5, 6, 7, 8), npki = c(1, 2, 3, 4, 5, 6, 7, 8), issignal = c(1, 2, 3, 4, 5, 6, 7, 8))
}
