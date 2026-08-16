% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.matern52}
\alias{.matern52}
\title{Matern 5/2 kernel}
\usage{
.matern52(d, amplitude = 1, length_scale = 1)
}
\description{
Elementwise on a distance \code{d} (vector or matrix). \eqn{k(d) = a^2 (1 + \sqrt{5} d/\ell + 5 d^2/(3 \ell^2)) \exp(-\sqrt{5} d/\ell)}{(a^2) * (1 + sqrt(5) d/l + 5 d^2/(3 l^2)) * exp(-sqrt(5) d/l)} (Rasmussen & Williams, GPML, Eq. 4.21).
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.matern52(V)
}
