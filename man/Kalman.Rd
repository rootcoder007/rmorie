% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Kalman}
\alias{Kalman}
\title{The recursive counterpart of the Wiener filter: it tracks a state}
\usage{
Kalman(z, F, H, Q, R, x0 = NULL, P0 = NULL) 
}
\arguments{
\item{z}{A matrix; passed to \code{as.matrix}.}
\item{F}{A matrix; passed to \code{as.matrix}.}
\item{H}{A matrix; passed to \code{as.matrix}.}
\item{Q}{A matrix; passed to \code{as.matrix}.}
\item{R}{A matrix; passed to \code{as.matrix}.}
\item{x0}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{P0}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
through a model instead of filtering a stationary record.  P is symmetrized each step for the same floating-point reason as in RLS, and the asymmetry it removed is returned.  The plain covariance update is used rather than the Joseph form, which is recorded because the Joseph form is the numerically safer one when R is small.
}
\value{
A list with \code{states}, \code{covariances}, \code{gains}, \code{innovations}, \code{n}, \code{state_dim}, \code{obs_dim}, \code{p_symmetry_error}, \code{p_symmetrized}, \code{joseph_form}, \code{method}.
}
\keyword{internal}
\examples{
F <- matrix(0.9)
H <- matrix(1)
Q <- matrix(0.1)
R <- matrix(1)
Riccati(F, H, Q, R)
Kalman(z = F, F = F, H = H, Q = Q, R = R)
}
