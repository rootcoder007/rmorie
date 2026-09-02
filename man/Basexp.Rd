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
beta(t) = sum_\{l=1\}^\{L1\} beta_l phi_l(t), the device that makes (14.1) estimable: an infinite-dimensional unknown function is replaced by L1 scalars, after which (14.1) collapses to the linear model (14.3).
}
\arguments{
\item{t}{Argument \code{t}; see Usage.}
\item{beta_coef}{Argument \code{beta_coef}; see Usage.}
\item{kind}{Argument \code{kind}; see Usage.}
\item{period}{Argument \code{period}; see Usage.}
}
\value{
A list with \code{beta_t}, \code{t}, \code{n_basis}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Basexp(V, V)
}
