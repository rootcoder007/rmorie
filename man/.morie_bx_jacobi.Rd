% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_jacobi}
\alias{.morie_bx_jacobi}
\title{Cyclic Jacobi rotations: the whole spectrum, repeated eigenvalues}
\usage{
.morie_bx_jacobi(S, sweeps = 60L, tol = 1e-12)
}
\arguments{
\item{S}{A matrix; passed to \code{nrow}.}
\item{sweeps}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{60L}.}
\item{tol}{Passed to \code{<=}. Defaults to \code{1e-12}.}
}
\description{
included, without the deflation error of power iteration
}
\value{
A list with \code{values}, \code{vectors}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_bx_jacobi(M)
}
