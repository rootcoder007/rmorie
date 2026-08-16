% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gee_poisson_exch}
\alias{.morie_gee_poisson_exch}
\title{.morie_gee_poisson_exch}
\usage{
.morie_gee_poisson_exch(X, y, id, max_iter = 50L, tol = 1e-08)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{id}{See Usage.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
GEE with Poisson family and exchangeable working correlation (Liang & Zeger 1986) -- the geepack::geeglm surface used by the OTIS batch module. Fisher scoring on the working model, moment estimate of the exchangeable alpha, robust (sandwich) covariance.
}
\value{
A list with \code{coefficients}, \code{vbeta}, \code{alpha}, \code{phi}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
\examples{
.morie_gee_poisson_exch(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), id = c("a", "b", "c"))
}
