% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{log_barrier_gradient}
\alias{log_barrier_gradient}
\title{Log barrier gradient}
\usage{
log_barrier_gradient(fvals, jac)
}
\arguments{
\item{fvals}{Numeric vector of \eqn{f_i(x)} values.}
\item{jac}{Numeric matrix of constraint gradients (one row per   constraint).}
}
\description{
\code{grad phi = sum grad f_i / (-f_i)} (Boyd p. 564).
}
\value{
Numeric vector of length \code{ncol(jac)}.
}
\keyword{internal}
