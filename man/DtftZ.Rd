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
\item{x}{See Usage.}
\item{omega}{See Usage.}
\item{fs}{Defaults to \code{NULL}.}
}
\description{
circle, z = exp(j omega T).  fs = NULL reads omega as normalized.
}
\value{
A list with \code{X}, \code{z}, \code{omega}, \code{T}, \code{n}, \code{on_unit_circle}, \code{method}.
}
\keyword{internal}
