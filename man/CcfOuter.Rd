% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CcfOuter}
\alias{CcfOuter}
\title{Eq (4.29): Theta_xy = E[x(n) y^T(n)], an N x N matrix carrying all}
\usage{
CcfOuter(x, y, order, tol = 0.001)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{order}{See Usage.}
\item{tol}{Defaults to \code{0.001}.}
}
\description{
pairwise delays -- which is why the outer product appears in the Wiener and RLS normal equations.  Under stationarity the entries depend only on i - j; that Toeplitz structure is MEASURED, and the deviation is the number to look at, not the boolean.
}
\value{
A list with \code{theta}, \code{order}, \code{n_positions}, \code{toeplitz_deviation}, \code{relative_deviation}, \code{tol}, \code{toeplitz}, \code{method}.
}
\keyword{internal}
