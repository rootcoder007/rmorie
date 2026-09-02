% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{policy_walk}
\alias{policy_walk}
\title{policy_walk}
\usage{
policy_walk(T, observations, gamma, n_iter = 1000, delta = 0.25, alpha = 1, 
    prior = "uniform", scale = 1, r_max = 1, J = 0.1, H = 0, burn = NULL, 
    seed = 0, R0 = NULL) 
}
\arguments{
\item{T}{Passed to \code{.mdp}.}
\item{observations}{Passed to \code{.birl_log_likelihood}.}
\item{gamma}{Passed to \code{.mdp}.}
\item{n_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{1000}.}
\item{delta}{Numeric; combined arithmetically in the body. Defaults to \code{0.25}.}
\item{alpha}{Passed to \code{.birl_log_likelihood}. Defaults to \code{1}.}
\item{prior}{Passed to \code{log_prior}. Defaults to \code{"uniform"}.}
\item{scale}{Passed to \code{log_prior}. Defaults to \code{1}.}
\item{r_max}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{J}{Passed to \code{log_prior}. Defaults to \code{0.1}.}
\item{H}{Passed to \code{log_prior}. Defaults to \code{0}.}
\item{burn}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{R0}{Optional; may be \code{NULL}. Iterated over elementwise, with \code{vapply}.}
}
\description{
A step of the birl_native implementation. Called by \code{birl}. See the file header for the source the module follows.
}
\value{
A list with \code{samples}, \code{acceptance}, \code{policy_iterations}, \code{n_proposals}, \code{final_policy}.
}
\keyword{internal}
\examples{
T <- list(list(c(0.9, 0.1), c(0.2, 0.8)), list(c(0.5, 0.5), c(0.1, 0.9)))
obs <- list(c(1L, 1L), c(2L, 2L), c(1L, 1L))
r <- policy_walk(T, obs, gamma = 0.9, n_iter = 100, seed = 1)
str(r, max.level = 1)
}
