% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_rl_out}
\alias{.mor_rl_out}
\title{.mor_rl_out}
\usage{
.mor_rl_out(Q, S, A)
}
\arguments{
\item{Q}{A matrix; indexed by row and column.}
\item{S}{A count; the body uses it as \code{seq_len(...)}.}
\item{A}{Passed to \code{.mor_rl_greedy}.}
}
\description{
A step of the qlearn_native implementation. Called by \code{morie_ddqn}, \code{morie_qlearn}, \code{morie_sarsa}. See the file header for the source the module follows.
}
\value{
A list with \code{policy}, \code{v}.
}
\keyword{internal}
