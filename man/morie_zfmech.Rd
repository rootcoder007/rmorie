% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_zfmech}
\alias{morie_zfmech}
\title{Release q(x) under rho-zCDP}
\usage{
morie_zfmech(value, sensitivity, rho, seed = 0, n = 1L)
}
\arguments{
\item{value}{The true query value q(x).}
\item{sensitivity}{Sensitivity Delta, must be non-negative.}
\item{rho}{zCDP parameter, must be positive.}
\item{seed}{Seed for the shared generator.}
\item{n}{Number of releases to draw, a positive integer.}
}
\description{
Draws Box-Muller cosine-branch normals from the shared generator using two uniforms per variate, with u1 clamped at 1e-12 to match the Python arm\'s safeguard against log(0). For \code{n = 1L} the release is a scalar; for \code{n > 1L} it is a numeric vector of length \code{n}, matching the Python list payload.
}
\value{
A list with \code{release}, \code{sigma}, \code{rho},   \code{sensitivity}.
}
\keyword{internal}
\examples{
morie_zfmech(value = c(1, 2, 3, 4, 5, 6, 7, 8), sensitivity = 5L, rho = 0.5)
}
