% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{planar_flow}
\alias{planar_flow}
\title{Planar flow \code{z + u*tanh(w\'z + b)} with its log-determinant}
\usage{
planar_flow(z, u, w, b)
}
\arguments{
\item{z,u,w}{Numeric vectors of equal length.}
\item{b}{Numeric scalar.}
}
\description{
The Jacobian is rank-one, so the matrix determinant lemma gives \code{|1 + u\'psi(z)|} in O(d) rather than O(d^3).
}
\value{
A list with \code{z, log_det, det, invertibility_adjusted,   note}.
}
\keyword{internal}
\examples{
planar_flow(z = c(1, 2, 3, 4, 5, 6, 7, 8), u = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
}
