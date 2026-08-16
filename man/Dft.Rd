% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dft}
\alias{Dft}
\title{Eq (3.80), evaluated straight from the definition: exact at any N,}
\usage{
Dft(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
with no power-of-two requirement.  eq (3.85) is the same sum split into cos and sin parts, returned here so the two cannot disagree.
}
\value{
A list with \code{X}, \code{real}, \code{imag}, \code{n}, \code{magnitude}, \code{conjugate_symmetric}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Dft(V)
}
