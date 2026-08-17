% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{policy_values}
\alias{policy_values}
\title{policy_values}
\usage{
policy_values(T, R, gamma, policy)
}
\arguments{
\item{T}{A vector; indexed elementwise.}
\item{R}{A vector; its length is taken.}
\item{gamma}{Numeric; combined arithmetically in the body.}
\item{policy}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the birl_native implementation. Called by \code{policy_iteration}, \code{policy_walk}. See the file header for the source the module follows.
}
\value{
The value of \code{.solve}.
}
\keyword{internal}
\examples{
T <- list(list(c(0.9, 0.1), c(0.2, 0.8)), list(c(0.5, 0.5), c(0.1, 0.9)))
policy_values(T, R = c(1, 0), gamma = 0.9, policy = c(1L, 2L))
}
