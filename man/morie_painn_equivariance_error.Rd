% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_painn_equivariance_error}
\alias{morie_painn_equivariance_error}
\title{morie_painn_equivariance_error}
\usage{
morie_painn_equivariance_error(model, s, v, R, Q, tol = 1e-09) 
}
\arguments{
\item{model}{Accepted by the signature and not used anywhere in the body.}
\item{s}{Passed to \code{model}.}
\item{v}{Passed to \code{.painn_mat}.}
\item{R}{Passed to \code{.painn_mat}.}
\item{Q}{Passed to \code{.painn_mat}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.}
}
\description{
A step of the painn_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{scalar_error}, \code{vector_error}, \code{scalars_invariant}, \code{vectors_equivariant}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
F <- 4
s <- rnorm(F)
v <- matrix(rnorm(3 * F), 3, F)
R <- matrix(rnorm(9), 3, 3)
th <- pi / 5
Q <- matrix(c(cos(th), -sin(th), 0, sin(th), cos(th), 0, 0, 0, 1), 3, 3)
model <- function(s, V, pos) list(s = s, v = V)
e <- morie_painn_equivariance_error(model, s, v, R, Q)
is.list(e)
}
