% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{policy_iteration}
\alias{policy_iteration}
\title{policy_iteration}
\usage{
policy_iteration(T, R, gamma, policy = NULL, max_iter = 200)
}
\arguments{
\item{T}{See Usage.}
\item{R}{See Usage.}
\item{gamma}{See Usage.}
\item{policy}{Defaults to \code{NULL}.}
\item{max_iter}{Defaults to \code{200}.}
}
\description{
Part of the birl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{policy}, \code{V}, \code{Q}, \code{sweeps}.
}
\keyword{internal}
