% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fda_basis}
\alias{morie_fda_basis}
\title{Functional basis matrix (MVSML eq. 14.5)}
\usage{
morie_fda_basis(t, n_basis, kind = "fourier", period = NULL) 
}
\arguments{
\item{t}{numeric grid of observation points}
\item{n_basis}{number of basis functions}
\item{kind}{"fourier" (default), or "poly" / "polynomial"}
\item{period}{Fourier period; defaults to the range of `t`}
}
\description{
Evaluates the first `n_basis` Fourier (or monomial) basis functions on the observation grid, the Psi of the basis expansion x(t) = sum_l c_l phi_l(t).
}
\value{
numeric matrix with one row per grid point
}
\keyword{internal}
\examples{
rmorie:::morie_fda_basis(t = c(1, 2, 3, 4, 5, 6, 7, 8), n_basis = 5L)
}
