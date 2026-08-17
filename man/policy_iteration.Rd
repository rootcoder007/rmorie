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
\item{T}{Passed to \code{.mdp}.}
\item{R}{A vector; its length is taken.}
\item{gamma}{Passed to \code{.mdp}.}
\item{policy}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200}.}
}
\description{
A step of the birl_native implementation. Called by \code{birl}, \code{policy_walk}. See the file header for the source the module follows.
}
\value{
A list with \code{policy}, \code{V}, \code{Q}, \code{sweeps}.
}
\keyword{internal}
\examples{
T <- list(list(c(0.9, 0.1), c(0.2, 0.8)), list(c(0.5, 0.5), c(0.1, 0.9)))
r <- policy_iteration(T, R = c(1, 0), gamma = 0.9)
str(r, max.level = 1)
}
