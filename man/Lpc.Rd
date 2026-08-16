% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lpc}
\alias{Lpc}
\title{Eqs (7.17)-(7.18), (7.25), (7.35).  The ACF is the BIASED estimator}
\usage{
Lpc(x, order, method = "autocorrelation")
}
\arguments{
\item{x}{See Usage.}
\item{order}{See Usage.}
\item{method}{Defaults to \code{"autocorrelation"}.}
}
\description{
(divide by N): that is what makes the Toeplitz system positive-definite and hence the model stable; 1/(N-m) does not.
}
\value{
A list with \code{a}, \code{gain}, \code{error}, \code{reflection}, \code{acf}, \code{order}, \code{residual}, \code{residual_energy}, \code{stable}, \code{normalized_error}, \code{sign_convention}, \code{method}.
}
\keyword{internal}
