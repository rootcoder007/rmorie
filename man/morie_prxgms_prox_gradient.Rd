% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prxgms_prox_gradient}
\alias{morie_prxgms_prox_gradient}
\title{morie_prxgms_prox_gradient}
\usage{
morie_prxgms_prox_gradient(fun, grad, prox, x0, L = 1, max.iter = 500L, tol = 1e-10, accelerate = TRUE, backtrack = FALSE, eta = 2, g.fun = NULL)
}
\arguments{
\item{fun}{See Usage.}
\item{grad}{See Usage.}
\item{prox}{See Usage.}
\item{x0}{See Usage.}
\item{L}{Defaults to \code{1}.}
\item{max.iter}{Defaults to \code{500L}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{accelerate}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{backtrack}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{eta}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
\item{g.fun}{Defaults to \code{NULL}.}
}
\description{
A step of the prxgms_native implementation. Called by \code{morie_prxgms_lasso_fista}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{x}, \code{fun}, \code{objective}, \code{iterations}, \code{L}, \code{accelerated}, \code{converged}, \code{method}.
}
\keyword{internal}
