% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_rl_eps}
\alias{.mor_rl_eps}
\title{Epsilon-greedy action (1-based) consuming one or two uniforms}
\usage{
.mor_rl_eps(e, Qs, A, epsilon)
}
\arguments{
\item{e}{Passed to \code{.ghc_unif}.}
\item{Qs}{Passed to \code{.mor_rl_greedy}.}
\item{A}{Numeric; combined arithmetically in the body.}
\item{epsilon}{Passed to \code{<}.}
}
\description{
A step of the qlearn_native implementation. Called by \code{morie_qlearn}, \code{morie_sarsa}. See the file header for the source the module follows.
}
\value{
The value of \code{.mor_rl_greedy}.
}
\keyword{internal}
