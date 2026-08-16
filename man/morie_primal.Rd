% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_primal}
\alias{morie_primal}
\title{morie_primal}
\usage{
morie_primal(K, Kt, prox_f_star, prox_g, x0, y0, tau = NULL, sigma = NULL, theta = 1, norm_K = NULL, max_iter = 500, tol = 1e-10)
}
\arguments{
\item{K}{See Usage.}
\item{Kt}{See Usage.}
\item{prox_f_star}{See Usage.}
\item{prox_g}{See Usage.}
\item{x0}{See Usage.}
\item{y0}{See Usage.}
\item{tau}{Defaults to \code{NULL}.}
\item{sigma}{Defaults to \code{NULL}.}
\item{theta}{Defaults to \code{1}.}
\item{norm_K}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{500}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
Part of the primal_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
