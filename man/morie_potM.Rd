% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_potM}
\alias{morie_potM}
\title{morie_potM}
\usage{
morie_potM(y, u, return_periods = c(10, 100))
}
\arguments{
\item{y}{See Usage.}
\item{u}{See Usage.}
\item{return_periods}{Defaults to \code{c(10, 100)}.}
}
\description{
Part of the potM_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{sigma}, \code{xi}, \code{loglik}, \code{cov}, \code{n_exceedances}, \code{n}, \code{rate}, \code{return_levels}, \code{threshold}, \code{converged}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_potM(V, M)
}
