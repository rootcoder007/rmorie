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
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{id}{See Usage.}
\item{max_iter}{Defaults to \code{50L}.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
GEE with Poisson family and exchangeable working correlation (Liang & Zeger 1986) -- the geepack::geeglm surface used by the OTIS batch module. Fisher scoring on the working model, moment estimate of the exchangeable alpha, robust (sandwich) covariance.
}
\value{
A list with \code{coefficients}, \code{vbeta}, \code{alpha}, \code{phi}, \code{n_iter}, \code{converged}.
}
\keyword{internal}
