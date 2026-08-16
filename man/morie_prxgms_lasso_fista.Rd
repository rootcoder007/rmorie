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
\item{A}{See Usage.}
\item{b}{See Usage.}
\item{lam}{See Usage.}
\item{max.iter}{Defaults to \code{500L}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{accelerate}{Defaults to \code{TRUE}.}
}
\description{
Part of the prxgms_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{res}, as built in the body.
}
\keyword{internal}
\examples{
morie_prxgms_lasso_fista(A = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, lam = 5L)
}
