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
\item{y}{See Usage.}
\item{K}{See Usage.}
\item{C}{Defaults to \code{1}.}
\item{tol}{Defaults to \code{0.001}.}
\item{eps}{Defaults to \code{1e-05}.}
\item{max_passes}{Defaults to \code{200}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the smoopt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{alpha}, \code{b}, \code{passes}, \code{full_passes}, \code{non_bound_passes}, \code{steps}, \code{support_vectors}, \code{n_sv}, \code{equality_residual}, \code{kkt_violations}, \code{objective}, \code{method}, \code{note}.
}
\keyword{internal}
