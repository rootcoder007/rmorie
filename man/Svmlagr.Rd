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
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{beta0}{Argument \code{beta0}; see Usage.}
\item{beta}{Argument \code{beta}; see Usage.}
\item{alpha}{Argument \code{alpha}; see Usage.}
}
\value{
A list with \code{L}, \code{quadratic_term}, \code{slack}, \code{grad_beta}, \code{grad_beta0}.
}
\keyword{internal}
\examples{
rmorie:::Svmlagr(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5, alpha = 0.5)
}
