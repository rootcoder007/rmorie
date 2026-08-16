% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_simulate_unconditional}
\alias{.schab_simulate_unconditional}
\title{.schab_simulate_unconditional}
\usage{
.schab_simulate_unconditional(mean, cov, method = "cholesky", seed = 0, stream = 0)
}
\arguments{
\item{mean}{A vector; its length is taken.}
\item{cov}{A matrix; passed to \code{dim}.}
\item{method}{The body requires: `method` must be 'cholesky' or 'spectral'. Defaults to \code{"cholesky"}.}
\item{seed}{Passed to \code{.morie_random_normal}. Defaults to \code{0}.}
\item{stream}{Passed to \code{.morie_random_normal}. Defaults to \code{0}.}
}
\description{
A step of the schab_sim_shared implementation. Called by \code{.schab_simulate_conditional}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
.schab_simulate_unconditional(mean = c("a", "b", "c"), cov = matrix(c(4, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3))
}
