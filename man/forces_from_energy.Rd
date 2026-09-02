% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{forces_from_energy}
\alias{forces_from_energy}
\title{forces_from_energy}
\usage{
forces_from_energy(energy_fn, R, h = 1e-05) 
}
\arguments{
\item{energy_fn}{Accepted by the signature and not used anywhere in the body.}
\item{R}{Passed to \code{.schn_mat}.}
\item{h}{Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.}
}
\description{
A step of the schN_native implementation. Called by \code{invariance_error}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{forces}, \code{net_force}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
R <- matrix(rnorm(4 * 3), 4, 3)
energy_fn <- function(R) sum(R^2)
Fm <- forces_from_energy(energy_fn, R)
c(nrow(Fm$forces), max(abs(Fm$forces + 2 * R)) < 1e-3)
}
