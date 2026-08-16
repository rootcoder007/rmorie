% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{hindsr}
\alias{hindsr}
\title{hindsr}
\usage{
hindsr(episodes, actions = NULL, goals = NULL, strategy = "future", k = 4L, tol = 1e-06, reward_fn = NULL, state_to_goal = NULL, seed = 0L, history = NULL)
}
\arguments{
\item{episodes}{See Usage.}
\item{actions}{Defaults to \code{NULL}.}
\item{goals}{Defaults to \code{NULL}.}
\item{strategy}{Defaults to \code{"future"}.}
\item{k}{Defaults to \code{4L}.}
\item{tol}{Defaults to \code{1e-06}.}
\item{reward_fn}{Defaults to \code{NULL}.}
\item{state_to_goal}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0L}.}
\item{history}{Defaults to \code{NULL}.}
}
\description{
Part of the hindsr_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{transitions}, \code{n_transitions}, \code{n_original}, \code{n_relabelled}, \code{rewards}, \code{success_rate}, \code{strategy}, \code{k}, \code{n_episodes}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
hindsr(D)
}
