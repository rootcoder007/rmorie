% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_nmf}
\alias{morie_geron_nmf}
\title{Non-negative matrix factorization by multiplicative updates (Geron Ch 7, hmnmf)}
\usage{
morie_geron_nmf(X, n_components = 2, max_iter = 400, tol = 1e-06, seed = 42)
}
\arguments{
\item{X}{Non-negative matrix (m, p).}
\item{n_components}{Inner rank k.}
\item{max_iter,tol,seed}{Fit controls.}
}
\description{
Non-negative matrix factorization by multiplicative updates (Geron Ch 7, hmnmf)
}
\value{
A list with \code{W}, \code{H}, \code{reconstruction}, \code{reconstruction_error}, \code{relative_error}, \code{n_iter}, \code{n_components}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
