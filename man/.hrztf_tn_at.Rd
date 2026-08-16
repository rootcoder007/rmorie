% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrztf_tn_at}
\alias{.hrztf_tn_at}
\title{T_n evaluated at an observation.  Page 219: T_n(y) "is replaced with}
\usage{
.hrztf_tn_at(yv, ygrid, T, y2, y1)
}
\arguments{
\item{yv}{Numeric; combined arithmetically in the body.}
\item{ygrid}{A vector; its length is taken and its elements indexed.}
\item{T}{A vector; indexed elementwise.}
\item{y2}{Passed to \code{<}.}
\item{y1}{Passed to \code{>}.}
}
\description{
an arbitrarily large negative number if y < y2 and an arbitrarily large positive number if y > y1".  Inside [y2, y1] the grid values are interpolated.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
n <- 40
x <- cbind(sin(1.3 * seq_len(n)), cos(0.6 * seq_len(n)))
y <- exp(x[, 1] + 0.5 * x[, 2])
Hrztf(x, y, ny = 7L, nz = 7L, nu = 7L)$A_le_B
.hrztf_tn_at(yv = n, ygrid = n, T = n, y2 = n, y1 = n)
}
