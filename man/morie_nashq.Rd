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
\item{actions}{A vector; its length is taken and its elements indexed.}
\item{step}{See Usage.}
\item{rewards}{See Usage.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0.9}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{epsilon}{Passed to \code{.nashq_pick}. Defaults to \code{0.1}.}
\item{episodes}{Defaults to \code{500}.}
\item{horizon}{Defaults to \code{50}.}
\item{start}{Defaults to \code{NULL}.}
\item{selection}{Passed to \code{.nashq_select}. Defaults to \code{"global_optimal"}.}
\item{terminal}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
\item{agent}{Defaults to \code{0L}.}
\item{tol}{Passed to \code{.nashq_select}. Defaults to \code{1e-09}.}
}
\description{
A step of the nashq_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{policy}, \code{nash_values}, \code{stage_game_types}, \code{returns}, \code{mean_return_last}, \code{selection}, \code{method}.
}
\keyword{internal}
