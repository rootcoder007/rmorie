% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_nashq}
\alias{morie_nashq}
\title{morie_nashq}
\usage{
morie_nashq(states, actions, step, rewards, gamma = 0.9, alpha = 0.5, epsilon = 0.1, episodes = 500, horizon = 50, start = NULL, selection = "global_optimal", terminal = NULL, seed = 0, agent = 0L, tol = 1e-09)
}
\arguments{
\item{states}{See Usage.}
\item{actions}{See Usage.}
\item{step}{See Usage.}
\item{rewards}{See Usage.}
\item{gamma}{Defaults to \code{0.9}.}
\item{alpha}{Defaults to \code{0.5}.}
\item{epsilon}{Defaults to \code{0.1}.}
\item{episodes}{Defaults to \code{500}.}
\item{horizon}{Defaults to \code{50}.}
\item{start}{Defaults to \code{NULL}.}
\item{selection}{Defaults to \code{"global_optimal"}.}
\item{terminal}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
\item{agent}{Defaults to \code{0L}.}
\item{tol}{Defaults to \code{1e-09}.}
}
\description{
Part of the nashq_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{policy}, \code{nash_values}, \code{stage_game_types}, \code{returns}, \code{mean_return_last}, \code{selection}, \code{method}.
}
\keyword{internal}
