% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlheic}
\alias{morie_tlheic}
\title{morie_tlheic}
\usage{
morie_tlheic(psi_of_P = NULL, basis = NULL, D = NULL, score = NULL, weights = NULL, h = 1e-05, tol = 1e-04, ridge = 1e-08, mode = c("estimate", "verify", "grad", "deriv"))
}
\arguments{
\item{psi_of_P}{Passed to \code{numerical_derivative}.}
\item{basis}{Passed to \code{estimate_eic}.}
\item{D}{Passed to \code{gradient_inner_product}.}
\item{score}{Passed to \code{numerical_derivative}.}
\item{weights}{Passed to \code{numerical_derivative}.}
\item{h}{Passed to \code{numerical_derivative}. Defaults to \code{1e-05}.}
\item{tol}{Passed to \code{verify_gradient}. Defaults to \code{1e-04}.}
\item{ridge}{Passed to \code{estimate_eic}. Defaults to \code{1e-08}.}
\item{mode}{One of \code{"deriv"}, \code{"grad"}, \code{"verify"}.}
}
\description{
A step of the tlheic_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{estimate_eic}.
}
\keyword{internal}
\examples{
set.seed(1)
B <- cbind(1, matrix(rnorm(40), 20, 2))
psi <- function(w) sum(w * B[, 2])
r <- morie_tlheic(psi_of_P = psi, basis = B, mode = "estimate")
str(r, max.level = 1)
}
