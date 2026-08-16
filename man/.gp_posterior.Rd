% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gp_posterior}
\alias{.gp_posterior}
\title{GP posterior}
\usage{
.gp_posterior(X, y, Xnew, amplitude = 1, length_scale = 1, noise = 1e-08)
}
\description{
Standard Gaussian-process regression with a jittered kernel matrix for numerical stability: \eqn{K_{xx} + \sigma^2 I}, Cholesky factor \eqn{L}, posterior mean and standard deviation at \code{Xnew}. Returns \code{mu} and \code{sd}.
}
\value{
A list with \code{mu}, \code{sd}.
}
\keyword{internal}
