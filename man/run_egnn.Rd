% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{run_egnn}
\alias{run_egnn}
\title{run_egnn}
\usage{
run_egnn(H, X, layers, phi_e, phi_x, phi_h, A = NULL, C = NULL)
}
\arguments{
\item{H}{Iterated over elementwise, with \code{lapply}.}
\item{X}{Iterated over elementwise, with \code{lapply}.}
\item{layers}{Coerced to integer by the body, with \code{as.integer}.}
\item{phi_e}{Passed to \code{egcl}.}
\item{phi_x}{Passed to \code{egcl}.}
\item{phi_h}{Passed to \code{egcl}.}
\item{A}{Passed to \code{egcl}.}
\item{C}{Passed to \code{egcl}.}
}
\description{
A step of the egnnL_native implementation. Called by \code{e_gcn}, \code{egnn_layer}, \code{egnnlayer} and 5 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{H}, \code{X}, \code{layers}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
H <- lapply(1:4, function(i) rnorm(2))
X <- lapply(1:4, function(i) rnorm(3))
phi_e <- function(hi, hj, d2, a) c(hi + hj, d2)
phi_x <- function(m) sum(m) * 0.01
phi_h <- function(hi, agg) hi + 0.1 * agg[seq_along(hi)]
r <- run_egnn(H, X, 2L, phi_e, phi_x, phi_h)
str(r, max.level = 1)
}
