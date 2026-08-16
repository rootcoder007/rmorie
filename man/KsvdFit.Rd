% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{KsvdFit}
\alias{KsvdFit}
\title{KsvdFit}
\usage{
KsvdFit(Y, natoms, sparsity, maxiter = 15, tol = 1e-10, seed = 1)
}
\arguments{
\item{Y}{Passed to \code{.morie_bx_mat}.}
\item{natoms}{A count; the body uses it as \code{seq_len(...)}.}
\item{sparsity}{Passed to \code{.morie_bx_omp}.}
\item{maxiter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{15}.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-10}.}
\item{seed}{Passed to \code{.morie_bx_rng}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{dictionary}, \code{coefficients}, \code{error}, \code{iterations}, \code{method}.
}
\keyword{internal}
