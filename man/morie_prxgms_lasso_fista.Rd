% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prxgms_lasso_fista}
\alias{morie_prxgms_lasso_fista}
\title{morie_prxgms_lasso_fista}
\usage{
morie_prxgms_lasso_fista(A, b, lam, max.iter = 500L, tol = 1e-10, accelerate = TRUE) 
}
\arguments{
\item{A}{A matrix; passed to \code{as.matrix}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lam}{Numeric; combined arithmetically in the body.}
\item{max.iter}{Passed to \code{morie_prxgms_prox_gradient}. Defaults to \code{500L}.}
\item{tol}{Passed to \code{morie_prxgms_prox_gradient}. Defaults to \code{1e-10}.}
\item{accelerate}{Passed to \code{morie_prxgms_prox_gradient}. Defaults to \code{TRUE}.}
}
\description{
A step of the prxgms_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{res}, as built in the body.
}
\keyword{internal}
\examples{
morie_prxgms_lasso_fista(A = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, lam = 5L)
}
