% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DtftZ}
\alias{DtftZ}
\title{Eq (3.66): the Fourier transform is the z-transform on the unit}
\usage{
DtftZ(x, omega, fs = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{omega}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
circle, z = exp(j omega T).  fs = NULL reads omega as normalized.
}
\value{
A list with \code{X}, \code{z}, \code{omega}, \code{T}, \code{n}, \code{on_unit_circle}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DtftZ(V, V)
}
