% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{egnn_layer}
\alias{egnn_layer}
\title{Public names resolved by fn/_lazy_map.json}
\usage{
egnn_layer(H, X, layers, phi_e, phi_x, phi_h, A = NULL, C = NULL)
}
\arguments{
\item{H}{Passed to \code{run_egnn}.}
\item{X}{Passed to \code{run_egnn}.}
\item{layers}{Passed to \code{run_egnn}.}
\item{phi_e}{Passed to \code{run_egnn}.}
\item{phi_x}{Passed to \code{run_egnn}.}
\item{phi_h}{Passed to \code{run_egnn}.}
\item{A}{Passed to \code{run_egnn}.}
\item{C}{Passed to \code{run_egnn}.}
}
\description{
A step of the egnnL_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{run_egnn}.
}
\keyword{internal}
\examples{
set.seed(1)
H <- lapply(1:4, function(i) rnorm(2))
X <- lapply(1:4, function(i) rnorm(3))
phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
phi_x <- function(m) sum(m) * 0.01
phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
r <- egnn_layer(H, X, layers = 1L, phi_e, phi_x, phi_h)
length(r$H)
}
