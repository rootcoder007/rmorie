% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BilinUnwarp}
\alias{BilinUnwarp}
\title{Undo the prewarping: analog frequency back to digital}
\usage{
BilinUnwarp(Omega, T = 1) 
}
\arguments{
\item{Omega}{An analog frequency or vector of them.}
\item{T}{The sampling interval, which must be positive.}
}
\description{
Equation (3.142), the inverse of equation (3.141). The two compose to the identity and the round-trip error is returned as evidence rather than asserted. Every finite analog frequency lands strictly inside the open interval from minus pi to pi, which is what makes the inverse well defined.
}
\value{
A list with the digital frequency \code{omega}, the round-trip   error against equation (3.141), and whether every result fell   strictly inside the open interval.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
BilinUnwarp(V)
}
