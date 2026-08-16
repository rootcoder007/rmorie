% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DftRI}
\alias{DftRI}
\title{Eq (3.85): the real part is the projection onto the k-th cosine, the}
\usage{
DftRI(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
imaginary part is MINUS the projection onto the corresponding sine.
}
\value{
A list with \code{X}, \code{cos_projection}, \code{sin_projection}, \code{real}, \code{imag}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
DftRI(V)
}
