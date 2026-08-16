% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rmrl}
\alias{morie_rmrl}
\title{morie_rmrl}
\usage{
morie_rmrl(machines, states, actions, step, label, gamma = 0.9, alpha = 0.5, epsilon = 0.1, episodes = 500, horizon = 100, start = NULL, dead_end = NULL, seed = 0, task_order = NULL)
}
\arguments{
\item{machines}{See Usage.}
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
\item{task_order}{Defaults to \code{NULL}.}
}
\description{
Part of the rmrl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{policy}, \code{returns}, \code{mean_return_last}, \code{mean_return_first}, \code{n_qfunctions}, \code{episodes}, \code{method}.
}
\keyword{internal}
