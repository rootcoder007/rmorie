% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_stl_loess_at}
\alias{.mor_stl_loess_at}
\title{Loess fitted value at x0 (Cleveland et al. 1990, Sec. 2.1)}
\usage{
.mor_stl_loess_at(xs, ys, x0, q, degree, rho = NULL)
}
\arguments{
\item{xs}{A vector; its length is taken and its elements indexed.}
\item{ys}{A vector; indexed elementwise.}
\item{x0}{Numeric; combined arithmetically in the body.}
\item{q}{A count; the body uses it as \code{seq_len(...)}.}
\item{degree}{Passed to \code{==}.}
\item{rho}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
A step of the stlAn_native implementation. Called by \code{morie_stl_decompose}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
