% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{birl}
\alias{birl}
\title{birl}
\usage{
birl(T, observations, gamma = 0.9, n_iter = 1000, delta = 0.25, alpha = 1, prior = "uniform", scale = 1, r_max = 1, J = 0.1, H = 0, burn = NULL, seed = 0, R0 = NULL)
}
\arguments{
\item{T}{Passed to \code{.mdp}.}
\item{observations}{Iterated over elementwise, with \code{lapply}.}
\item{gamma}{Passed to \code{.mdp}. Defaults to \code{0.9}.}
\item{n_iter}{Passed to \code{policy_walk}. Defaults to \code{1000}.}
\item{delta}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.25}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{prior}{Carried through into a list the body builds. Defaults to \code{"uniform"}.}
\item{scale}{Passed to \code{policy_walk}. Defaults to \code{1}.}
\item{r_max}{Passed to \code{policy_walk}. Defaults to \code{1}.}
\item{J}{Passed to \code{policy_walk}. Defaults to \code{0.1}.}
\item{H}{Passed to \code{policy_walk}. Defaults to \code{0}.}
\item{burn}{Passed to \code{policy_walk}.}
\item{seed}{Passed to \code{policy_walk}. Defaults to \code{0}.}
\item{R0}{Passed to \code{policy_walk}.}
}
\description{
A step of the birl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{reward_mean}, \code{reward_sd}, \code{policy}, \code{V}, \code{Q}, \code{samples}, \code{acceptance}, \code{policy_iterations}, \code{n_proposals}, \code{n_samples}, \code{prior}, \code{alpha}, \code{delta}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
T <- list(list(c(0.9, 0.1), c(0.2, 0.8)), list(c(0.5, 0.5), c(0.1, 0.9)))
obs <- list(c(1L, 1L), c(2L, 2L), c(1L, 1L))
r <- birl(T, obs, gamma = 0.9, n_iter = 100, seed = 1)
str(r, max.level = 1)
}
