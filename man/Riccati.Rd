% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Riccati}
\alias{Riccati}
\title{The fixed point of the Kalman covariance recursion.  Once P settles}
\usage{
Riccati(F, H, Q, R, maxiter = 1000L, tol = 1e-12) 
}
\arguments{
\item{F}{A matrix; passed to \code{as.matrix}.}
\item{H}{A matrix; passed to \code{as.matrix}.}
\item{Q}{A matrix; passed to \code{as.matrix}.}
\item{R}{A matrix; passed to \code{as.matrix}.}
\item{maxiter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1000L}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-12}.}
}
\description{
the gain is constant and the filter is a fixed linear filter -- which is the Wiener solution for that model, and the sense in which Kalman generalizes Wiener.  Solved by iterating rather than by an eigenvalue method; a model that is not detectable has no stabilizing solution and will not converge, which `converged` reports instead of returning whatever P happened to be at the iteration limit.
}
\value{
A list with \code{P}, \code{K}, \code{iterations}, \code{change}, \code{converged}, \code{n}, \code{steady_state_is_the_wiener_solution}, \code{method}.
}
\keyword{internal}
\examples{
F <- matrix(0.9)
H <- matrix(1)
Q <- matrix(0.1)
R <- matrix(1)
Riccati(F, H, Q, R)
}
