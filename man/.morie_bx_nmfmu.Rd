% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_nmfmu}
\alias{.morie_bx_nmfmu}
\title{Lee-Seung multiplicative updates: "ls" is eqs (9.49)-(9.50), "kld" is}
\usage{
.morie_bx_nmfmu(V, r, maxiter, tol, seed, cost)
}
\arguments{
\item{V}{A matrix; indexed by row and column.}
\item{r}{A count; the body uses it as \code{seq_len(...)}.}
\item{maxiter}{See Usage.}
\item{tol}{Numeric; combined arithmetically in the body.}
\item{seed}{Passed to \code{.morie_bx_rng}.}
\item{cost}{See Usage.}
}
\description{
eqs (9.54)-(9.55)
}
\value{
A list with \code{W}, \code{H}, \code{error}, \code{iterations}.
}
\keyword{internal}
