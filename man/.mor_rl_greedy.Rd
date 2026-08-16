% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_rl_greedy}
\alias{.mor_rl_greedy}
\title{Argmax with the lowest index winning ties (Python _greedy)}
\usage{
.mor_rl_greedy(Qs, A)
}
\arguments{
\item{Qs}{A vector; indexed elementwise.}
\item{A}{Passed to \code{seq.int}.}
}
\description{
A step of the qlearn_native implementation. Called by \code{.mor_rl_eps}, \code{.mor_rl_out}, \code{morie_ddqn} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{b}, as built in the body.
}
\keyword{internal}
