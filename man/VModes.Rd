% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VModes}
\alias{VModes}
\title{Variational mode decomposition, Dragomiretskiy & Zosso (2014)}
\usage{
VModes(x, K = 3, alpha = 2000, tau = 0, init = "uniform", tol = 1e-07, max_iter = 300)
}
\arguments{
\item{x}{Passed to \code{.tf_need}.}
\item{K}{Defaults to \code{3}.}
\item{alpha}{Defaults to \code{2000}.}
\item{tau}{Defaults to \code{0}.}
\item{init}{Defaults to \code{"uniform"}.}
\item{tol}{Defaults to \code{1e-07}.}
\item{max_iter}{Defaults to \code{300}.}
}
\description{
A step of the rangayyan_tf implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{modes}, \code{center_freqs}, \code{K}, \code{alpha}, \code{tau}, \code{iterations}, \code{converged}, \code{reconstruction_error}, \code{residual_energy}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
VModes(V)
}
