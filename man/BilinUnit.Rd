% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BilinUnit}
\alias{BilinUnit}
\title{The unit circle maps onto the imaginary axis and nowhere else}
\usage{
BilinUnit(omega, T = 1)
}
\arguments{
\item{omega}{A digital frequency or vector of them, in radians.}
\item{T}{The sampling interval, which must be positive.}
}
\description{
Equation (3.140). On the unit circle the real part of the image vanishes exactly, so the mapping carries the unit circle onto the imaginary axis. The residual real part is computed and returned rather than assumed away, because it is the check that the mapping is doing what it claims.
}
\value{
A list with the image \code{s}, the largest residual real part   and the gap between the direct evaluation and the closed form, so a   reader can see how exactly the two agree.
}
\keyword{internal}
