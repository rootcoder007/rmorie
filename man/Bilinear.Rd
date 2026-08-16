% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Bilinear}
\alias{Bilinear}
\title{The bilinear transformation from the z plane to the s plane}
\usage{
Bilinear(z, T = 1)
}
\arguments{
\item{z}{A point or vector of points in the z plane; zero is outside   the domain.}
\item{T}{The sampling interval, which must be positive.}
}
\description{
Maps the whole left half-plane into the unit disc, so a stable analog filter always yields a stable digital one. Impulse invariance does not have that property -- it aliases -- and the price paid here instead is the frequency warping of equations (3.141) and (3.142). The point z = -1 maps to an infinite analog frequency and is refused.
}
\value{
A list with the image \code{s}, the inputs, and the three   properties that make the transformation worth using: that it maps   the left half-plane into the unit disc, that stability is preserved   and that nothing aliases.
}
\keyword{internal}
