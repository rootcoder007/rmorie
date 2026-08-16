% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smo}
\alias{smo}
\title{smo}
\usage{
smo(y, K, C = 1, tol = 1e-08, max_iter = 20000)
}
\arguments{
\item{y}{See Usage.}
\item{K}{See Usage.}
\item{C}{Defaults to \code{1}.}
\item{tol}{Defaults to \code{1e-08}.}
\item{max_iter}{Defaults to \code{20000}.}
}
\description{
Part of the svmopt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{alpha}, \code{b}, \code{gap}, \code{iterations}, \code{converged}, \code{support_vectors}, \code{n_sv}, \code{n_free}, \code{equality_residual}, \code{objective}, \code{method}, \code{note}.
}
\keyword{internal}
