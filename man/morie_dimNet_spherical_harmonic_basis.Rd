% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dimNet_spherical_harmonic_basis}
\alias{morie_dimNet_spherical_harmonic_basis}
\title{Spherical harmonic angular basis (m = 0)}
\usage{
morie_dimNet_spherical_harmonic_basis(angle, n_basis = 4L) 
}
\arguments{
\item{angle}{Numeric angle in radians.}
\item{n_basis}{Integer number of basis functions.}
}
\description{
Legendre polynomials in \code{cos(alpha)}, evaluated by the standard three-term recurrence. The \code{m = 0} spherical harmonics, orthogonal on the sphere.
}
\value{
Numeric vector of length \code{n_basis}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_dimNet_spherical_harmonic_basis(V)
}
