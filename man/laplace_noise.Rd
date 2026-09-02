% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{laplace_noise}
\alias{laplace_noise}
\title{One draw from Lap(0, b) by inverse transform}
\usage{
laplace_noise(scale, e) 
}
\arguments{
\item{scale}{Noise scale b.}
\item{e}{Generator environment from \code{.ghc_rng}.}
}
\description{
One draw from Lap(0, b) by inverse transform
}
\value{
A scalar.
}
\keyword{internal}
\examples{
laplace_noise(scale = 0.5, e = .ghc_rng(1))
}
