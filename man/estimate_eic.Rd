% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{estimate_eic}
\alias{estimate_eic}
\title{estimate_eic}
\usage{
estimate_eic(psi_of_P, basis, weights = NULL, h = 1e-05, ridge = 1e-08)
}
\arguments{
\item{psi_of_P}{Passed to \code{numerical_derivative}.}
\item{basis}{A matrix; passed to \code{as.matrix}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Passed to \code{numerical_derivative}. Defaults to \code{1e-05}.}
\item{ridge}{A matrix; passed to \code{diag}. Defaults to \code{1e-08}.}
}
\description{
A step of the tlheic_native implementation. Called by \code{morie_tlheic}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{D}, \code{coefficients}, \code{n_directions}, \code{mean}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
B <- cbind(1, matrix(rnorm(40), 20, 2))
psi <- function(w) sum(w * B[, 2])
r <- estimate_eic(psi, B)
str(r, max.level = 1)
}
