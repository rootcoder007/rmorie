% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Idft}
\alias{Idft}
\title{Eq (3.81): the 1/N and the PLUS sign are what distinguish it from the}
\usage{
Idft(X)
}
\arguments{
\item{X}{See Usage.}
}
\description{
forward transform of eq (3.80); getting either wrong scales the signal by N or reverses it in time, neither visible on a magnitude plot.
}
\value{
A list with \code{x}, \code{complex}, \code{n}, \code{max_imaginary}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Idft(V)
}
