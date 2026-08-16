% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rmrl_qlearn_flat}
\alias{morie_rmrl_qlearn_flat}
\title{morie_rmrl_qlearn_flat}
\usage{
morie_rmrl_qlearn_flat(machine, states, actions, step, label, gamma = 0.9, alpha = 0.5, epsilon = 0.1, episodes = 500, horizon = 100, start = NULL, dead_end = NULL, seed = 0)
}
\arguments{
\item{machine}{See Usage.}
\item{states}{See Usage.}
\item{actions}{See Usage.}
\item{step}{See Usage.}
\item{label}{See Usage.}
\item{gamma}{Defaults to \code{0.9}.}
\item{alpha}{Defaults to \code{0.5}.}
\item{epsilon}{Defaults to \code{0.1}.}
\item{episodes}{Defaults to \code{500}.}
\item{horizon}{Defaults to \code{100}.}
\item{start}{Defaults to \code{NULL}.}
\item{dead_end}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the rmrl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{returns}, \code{mean_return_last}, \code{mean_return_first}, \code{method}.
}
\keyword{internal}
