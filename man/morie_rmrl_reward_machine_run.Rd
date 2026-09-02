% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rmrl_reward_machine_run}
\alias{morie_rmrl_reward_machine_run}
\title{Drive a machine over a sequence of truth assignments. labels is}
\usage{
morie_rmrl_reward_machine_run(machine, labels) 
}
\arguments{
\item{machine}{A list; the body reads \code{$terminal}, \code{$u0} from it.}
\item{labels}{See Usage.}
}
\description{
sigma_0, sigma_1, ..., i.e. L(s) for each visited state. Returns the machine-state trajectory and the rewards emitted.
}
\value{
A list with \code{estimate}, \code{states}, \code{rewards}, \code{total_reward}, \code{final_state}, \code{accepted}, \code{method}.
}
\keyword{internal}
\examples{
edges <- list(list(0L, "a", 1L, 0), list(1L, "b", 2L, 1))
m <- morie_rmrl_reward_machine(edges, u0 = 0, terminal = c(2L))
r <- morie_rmrl_reward_machine_run(m, c("a", "b"))
str(r, max.level = 1)
}
