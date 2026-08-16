% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RlsUpdate}
\alias{RlsUpdate}
\title{Eq. (3.224): w(n) = w(n-1) + k(n) alpha(n)}
\usage{
RlsUpdate(w_prev, k, alpha)
}
\arguments{
\item{w_prev}{See Usage.}
\item{k}{See Usage.}
\item{alpha}{See Usage.}
}
\description{
Part of the rangayyan_adapt implementation; see the file header for the source it follows.
}
\value{
A list with \code{w_next}, \code{correction}, \code{alpha}, \code{order}, \code{sign}, \code{erratum}, \code{method}.
}
\keyword{internal}
