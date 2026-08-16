% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sp_rho_bounds}
\alias{.sp_rho_bounds}
\title{Internal: valid interval for a spatial autoregressive parameter.}
\usage{
.sp_rho_bounds(W, form = "identity")
}
\arguments{
\item{W}{Weights matrix (n by n).}
\item{form}{"identity" for I - rho W, "weighted" for D - rho W.}
}
\description{
A SAR/CAR/Durbin likelihood exists only where the implied precision (or |I - rho W|) is positive definite. The bound is an eigenvalue condition, rho in (1/theta_min, 1/theta_max), stated at eq (6.48). Hardcoding (-0.99, 0.99) instead is only safe when W is row-standardised AND the true bound is wider. For a raw adjacency it is not: on a 24-node chain the valid range is (-0.504, 0.504), so most of a (-0.99, 0.99) search lies where the likelihood is undefined. Note that a row-standardised W is ASYMMETRIC, and `eigen(symmetric = TRUE)` would read one triangle only; the matrix is symmetrised first.
}
\value{
Numeric length-2 vector (lo, hi), an open interval.
}
\keyword{internal}
\examples{
.sp_rho_bounds(W = 5L)
}
