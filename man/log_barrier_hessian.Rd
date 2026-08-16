% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{log_barrier_hessian}
\alias{log_barrier_hessian}
\title{Log barrier Hessian}
\usage{
log_barrier_hessian(fvals, jac, hess = NULL)
}
\arguments{
\item{fvals}{Numeric vector of \eqn{f_i(x)} values.}
\item{jac}{Numeric matrix of constraint gradients.}
\item{hess}{Optional list of constraint Hessians, one per   constraint; \code{NULL} entries are skipped (use for affine).}
}
\description{
\code{H phi = sum grad f_i grad f_i^T / f_i^2 + sum H f_i / (-f_i)} (Boyd p. 564). The second term is omitted when \code{hess} is \code{NULL}, which is the affine case.
}
\value{
Square numeric matrix.
}
\keyword{internal}
