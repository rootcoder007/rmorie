% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{policy_walk}
\alias{policy_walk}
\title{policy_walk}
\usage{
policy_walk(T, observations, gamma, n_iter = 1000, delta = 0.25, alpha = 1, prior = "uniform", scale = 1, r_max = 1, J = 0.1, H = 0, burn = NULL, seed = 0, R0 = NULL)
}
\arguments{
\item{T}{See Usage.}
\item{observations}{See Usage.}
\item{gamma}{See Usage.}
\item{n_iter}{Defaults to \code{1000}.}
\item{delta}{Defaults to \code{0.25}.}
\item{alpha}{Defaults to \code{1}.}
\item{prior}{Defaults to \code{"uniform"}.}
\item{scale}{Defaults to \code{1}.}
\item{r_max}{Defaults to \code{1}.}
\item{J}{Defaults to \code{0.1}.}
\item{H}{Defaults to \code{0}.}
\item{burn}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
\item{R0}{Defaults to \code{NULL}.}
}
\description{
Part of the birl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{samples}, \code{acceptance}, \code{policy_iterations}, \code{n_proposals}, \code{final_policy}.
}
\keyword{internal}
