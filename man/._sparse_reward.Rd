% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{._sparse_reward}
\alias{._sparse_reward}
\title{._sparse_reward}
\usage{
._sparse_reward(s, a, s_next, g, tol)
}
\arguments{
\item{s}{Accepted by the signature and not used anywhere in the body.}
\item{a}{Accepted by the signature and not used anywhere in the body.}
\item{s_next}{A vector; its length is taken and its elements indexed.}
\item{g}{A vector; its length is taken and its elements indexed.}
\item{tol}{Passed to \code{>}.}
}
\description{
A step of the hindsr_native implementation. Called by \code{hindsr}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
._sparse_reward(s = c(1, 2, 3, 4, 5, 6, 7, 8), a = c(1, 2, 3, 4, 5, 6, 7, 8), s_next = c(1, 2, 3, 4, 5, 6, 7, 8), g = c(1, 2, 3, 4, 5, 6, 7, 8), tol = 0.5)
}
