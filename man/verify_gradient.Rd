% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{verify_gradient}
\alias{verify_gradient}
\title{verify_gradient}
\usage{
verify_gradient(psi_of_P, D, score, weights = NULL, h = 1e-05, tol = 1e-04)
}
\arguments{
\item{psi_of_P}{Passed to \code{numerical_derivative}.}
\item{D}{A vector; its length is taken.}
\item{score}{Passed to \code{numerical_derivative}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Passed to \code{numerical_derivative}. Defaults to \code{1e-05}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-04}.}
}
\description{
A step of the tlheic_native implementation. Called by \code{morie_tlheic}. See the file header for the source the module follows.
}
\value{
A list with \code{derivative}, \code{inner_product}, \code{difference}, \code{verified}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
B <- cbind(1, matrix(rnorm(40), 20, 2))
psi <- function(w) sum(w * B[, 2])
eic <- estimate_eic(psi, B)
score <- B[, 2] - mean(B[, 2])
r <- verify_gradient(psi, eic$D, score)
str(r, max.level = 1)
}
