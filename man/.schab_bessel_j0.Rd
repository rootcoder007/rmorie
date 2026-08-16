% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_bessel_j0}
\alias{.schab_bessel_j0}
\title{J_0(x) = (1/pi) integral_0^pi cos(x sin theta) dtheta. The integrand}
\usage{
.schab_bessel_j0(x, n_quad = 200L)
}
\arguments{
\item{x}{See Usage.}
\item{n_quad}{Defaults to \code{200L}.}
}
\description{
is smooth and periodic so the trapezoid rule converges geometrically; base R\\'s besselJ() is deliberately not used, see the file header.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
