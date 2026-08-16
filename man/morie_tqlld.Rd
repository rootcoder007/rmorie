% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tqlld}
\alias{morie_tqlld}
\title{Optimal levels-point scalar codebook}
\usage{
morie_tqlld(levels = 4, source = "gaussian", data = NULL, lo = NULL, hi = NULL, max_iter = 200, tol = 1e-12, n_grid = 20000)
}
\arguments{
\item{levels}{Number of codewords.}
\item{source}{One of \code{"gaussian"}, \code{"empirical"},   \code{"uniform"}.}
\item{data}{Samples for the empirical source.}
\item{lo}{Lower bound for the uniform source.}
\item{hi}{Upper bound for the uniform source.}
\item{max_iter}{Maximum iterations.}
\item{tol}{Convergence tolerance.}
\item{n_grid}{Quadrature grid size for the Gaussian source.}
}
\description{
Optimal levels-point scalar codebook
}
\value{
A list with \code{estimate}, \code{codebook},   \code{boundaries}, \code{distortion},   \code{distortion_history}, \code{iterations}, \code{converged},   \code{source}, \code{levels}, \code{lo}, \code{hi} (uniform   only), \code{method}.
}
\keyword{internal}
\examples{
morie_tqlld()
}
