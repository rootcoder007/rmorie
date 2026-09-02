% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{equivariantgraphconv}
\alias{equivariantgraphconv}
\title{Compact alias per ledger/NAMING.md}
\usage{
equivariantgraphconv(H, X, layers, phi_e, phi_x, phi_h, A = NULL, C = NULL) 
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
A step of the egcn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{run_egnn}.
}
\keyword{internal}
\examples{
set.seed(1)
r <- equivariantgraphconv(H = rnorm(10), X = rnorm(10), layers = rnorm(10), phi_e = rnorm(10), phi_x = rnorm(10), phi_h = rnorm(10)); TRUE
}
