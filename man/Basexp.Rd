% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Basexp}
\alias{Basexp}
\title{Basis expansion of the coefficient function (MVSML eq. 14.2)}
\usage{
Basexp(t, beta_coef, kind = "fourier", period = NULL)
}
\description{
beta(t) = sum_{l=1}^{L1} beta_l phi_l(t), the device that makes (14.1) estimable: an infinite-dimensional unknown function is replaced by L1 scalars, after which (14.1) collapses to the linear model (14.3).
}
\value{
A list with \code{beta_t}, \code{t}, \code{n_basis}.
}
\keyword{internal}
