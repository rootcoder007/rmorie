% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Svmlagr}
\alias{Svmlagr}
\title{Wolfe primal of the maximum margin problem (MVSML eq. 9.27)}
\usage{
Svmlagr(X, y, beta0, beta, alpha)
}
\description{
L = (1/2)||beta||^2 - sum_i alpha_i [ y_i(beta_0 + x_i beta) - 1 ]. Its derivatives with respect to beta and beta_0 are (9.28) and (9.29), both zero at the optimum.
}
\value{
A list with \code{L}, \code{quadratic_term}, \code{slack}, \code{grad_beta}, \code{grad_beta0}.
}
\keyword{internal}
