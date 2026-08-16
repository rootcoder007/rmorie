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
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{tau}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{sigma}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{theta}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{norm_K}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{500}.}
\item{tol}{Defaults to \code{1e-10}.}
}
\description{
A step of the primal_native implementation. Called by \code{morie_tv_denoise_1d}. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
