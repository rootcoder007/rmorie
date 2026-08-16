% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Penmat}
\alias{Penmat}
\title{Roughness penalty matrix (MVSML eq. 14.11)}
\usage{
Penmat(t, L1, p = 2L, kind = "fourier", period = NULL, beta = NULL)
}
\description{
J_beta = int_0^T [ d^p beta(t) / dt^p ]^2 dt.  Under the basis expansion (14.2) the book writes J_beta = beta\' P beta with P_ij = int_0^T phi_i^(p)(t) phi_j^(p)(t) dt.  The chapter says p is typically 1 or 2.  Integrals by the trapezoid rule on the grid t.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
