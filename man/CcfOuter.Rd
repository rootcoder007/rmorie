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
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.}
}
\description{
pairwise delays -- which is why the outer product appears in the Wiener and RLS normal equations.  Under stationarity the entries depend only on i - j; that Toeplitz structure is MEASURED, and the deviation is the number to look at, not the boolean.
}
\value{
A list with \code{theta}, \code{order}, \code{n_positions}, \code{toeplitz_deviation}, \code{relative_deviation}, \code{tol}, \code{toeplitz}, \code{method}.
}
\keyword{internal}
\examples{
CcfOuter(x = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), order = 5L)
}
