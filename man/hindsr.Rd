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
\item{episodes}{Passed to \code{._as_states}.}
\item{actions}{Optional; may be \code{NULL}. Iterated over elementwise, with \code{lapply}.}
\item{goals}{Optional; may be \code{NULL}. Iterated over elementwise, with \code{lapply}.}
\item{strategy}{Passed to \code{._sample_goals}. Defaults to \code{"future"}.}
\item{k}{Passed to \code{._sample_goals}. Defaults to \code{4L}.}
\item{tol}{Passed to \code{._sparse_reward}. Defaults to \code{1e-06}.}
\item{reward_fn}{Optional; may be \code{NULL}. A function; the body checks with \code{is.function}.}
\item{state_to_goal}{Defaults to \code{NULL}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0L}.}
\item{history}{Optional; may be \code{NULL}. Passed to \code{._as_states}.}
}
\description{
A step of the hindsr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{transitions}, \code{n_transitions}, \code{n_original}, \code{n_relabelled}, \code{rewards}, \code{success_rate}, \code{strategy}, \code{k}, \code{n_episodes}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
hindsr(D)
}
