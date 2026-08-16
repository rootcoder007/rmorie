% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fda_basis_deriv}
\alias{morie_fda_basis_deriv}
\title{P-th derivative of the basis functions of (14.2).  For the Fourier}
\usage{
morie_fda_basis_deriv(t, n_basis, p = 1L, kind = "fourier", period = NULL)
}
\arguments{
\item{t}{See Usage.}
\item{n_basis}{See Usage.}
\item{p}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1L}.}
\item{kind}{Defaults to \code{"fourier"}.}
\item{period}{Defaults to \code{NULL}.}
}
\description{
basis phi_0 = 1, phi_{2k-1} = sin(2 pi k t / P) and phi_{2k} = cos(2 pi k t / P), so the p-th derivative is (2 pi k / P)^p times a quarter-period phase shift.  Mirrors the Python _gp_core.fda_basis_derivative, including its polynomial convention u = (t - lo) / span, which differs from morie_fda_basis above.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_fda_basis_deriv(V, V)
}
