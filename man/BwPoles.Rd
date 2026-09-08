% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwPoles}
\alias{BwPoles}
\title{Eq (3.137): all 2N poles on a circle of radius Omega_c, spaced pi/N}
\usage{
BwPoles(Omega_c, N, k = NULL) 
}
\arguments{
\item{Omega_c}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{N}{Coerced to integer by the body, with \code{as.integer}.}
\item{k}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
apart, symmetric about the imaginary axis and never on it.  For odd N a pole falls on the real axis.  Complex poles come in conjugate pairs, which keeps the filter coefficients real.
}
\value{
A list with \code{poles}, \code{left_half_plane}, \code{value}, \code{k}, \code{Omega_c}, \code{N}, \code{radius}, \code{angular_spacing}, \code{n_left_half_plane}, \code{none_on_the_imaginary_axis}, \code{real_pole_for_odd_order}, \code{method}.
}
\keyword{internal}
\examples{
BwPoles(Omega_c = 5L, N = 5L)
}
