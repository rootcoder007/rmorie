% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ObsReal}
\alias{ObsReal}
\title{Eq (3.95): y_k(n) = x_k(n) + eta_k(n), the model that synchronized}
\usage{
ObsReal(x, eta)
}
\arguments{
\item{x}{See Usage.}
\item{eta}{See Usage.}
}
\description{
averaging assumes.  A single x is read as the same signal repeated, which is the book\\'s "identical and aligned" case.
}
\value{
A list with \code{y}, \code{m}, \code{n}, \code{identical_repetitions}, \code{method}.
}
\keyword{internal}
