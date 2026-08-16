% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smo_platt}
\alias{smo_platt}
\title{smo_platt}
\usage{
smo_platt(y, K, C = 1, tol = 0.001, eps = 1e-05, max_passes = 200, seed = 0)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{K}{A matrix; indexed by row and column.}
\item{C}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{tol}{Defaults to \code{0.001}.}
\item{eps}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-05}.}
\item{max_passes}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{seed}{Passed to \code{.smoopt_make_rng}. Defaults to \code{0}.}
}
\description{
A step of the smoopt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{alpha}, \code{b}, \code{passes}, \code{full_passes}, \code{non_bound_passes}, \code{steps}, \code{support_vectors}, \code{n_sv}, \code{equality_residual}, \code{kkt_violations}, \code{objective}, \code{method}, \code{note}.
}
\keyword{internal}
