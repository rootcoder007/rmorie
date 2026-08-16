% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mtdrl_bandit_tasks}
\alias{morie_mtdrl_bandit_tasks}
\title{A distribution of bandit tasks}
\usage{
morie_mtdrl_bandit_tasks(n_arms = 2, n_tasks = 100, seed = 0, structure = "independent")
}
\arguments{
\item{n_arms}{Number of arms.}
\item{n_tasks}{Number of tasks.}
\item{seed}{Seed for the shared generator.}
\item{structure}{"independent" or "paired".}
}
\description{
A distribution of bandit tasks
}
\value{
A list of per-task arm-probability vectors.
}
\keyword{internal}
\examples{
morie_mtdrl_bandit_tasks()
}
