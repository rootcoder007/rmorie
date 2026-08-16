% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dpgrf_car_precision}
\alias{.dpgrf_car_precision}
\title{Conditional autoregressive precision matrix}
\usage{
.dpgrf_car_precision(W, rho = 0.99, tau = 1)
}
\arguments{
\item{W}{Square symmetric adjacency matrix (n x n).}
\item{rho}{Spatial correlation in \code{[0, 1]}.}
\item{tau}{Positive scale.}
}
\description{
Returns \code{tau * (D - rho * W)} with \code{D} the diagonal of row sums of \code{W}. With \code{rho = 1} the matrix is the intrinsic CAR and is singular, which the model has to live with rather than hide.
}
\value{
A list with \code{Q}, \code{degrees}, \code{rho},   \code{min_eigenvalue}, \code{singular}, \code{note}.
}
\keyword{internal}
