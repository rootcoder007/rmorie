% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsHpLpTf}
\alias{QrsHpLpTf}
\title{Eq (4.9): Hlp(z) = (1 - z^-32)/(1 - z^-1), a running sum of 32}
\usage{
QrsHpLpTf(freq, fs = 200)
}
\arguments{
\item{freq}{See Usage.}
\item{fs}{Numeric; combined arithmetically in the body. Defaults to \code{200}.}
}
\description{
samples Evaluated as sum_{k=0}^{31} z^-k so the z = 1 point is exact.
}
\value{
A list with \code{freq}, \code{mag}, \code{phase}, \code{b}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
QrsHpLpTf(V)
}
