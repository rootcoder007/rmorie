% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dimNet_bessel_basis}
\alias{morie_dimNet_bessel_basis}
\title{Spherical Bessel radial basis}
\usage{
morie_dimNet_bessel_basis(d, cutoff = 5, n_basis = 8L)
}
\arguments{
\item{d}{Numeric distance.}
\item{cutoff}{Numeric cutoff.}
\item{n_basis}{Integer number of basis functions.}
}
\description{
\code{sqrt(2/c) sin(n pi d/c) / d}, orthogonal on \code{[0, c]}.
}
\value{
Numeric vector of length \code{n_basis}.
}
\keyword{internal}
