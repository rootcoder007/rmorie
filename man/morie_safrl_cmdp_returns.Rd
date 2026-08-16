% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_safrl_cmdp_returns}
\alias{morie_safrl_cmdp_returns}
\title{morie_safrl_cmdp_returns}
\usage{
morie_safrl_cmdp_returns(policy, states, actions, step, reward, costs, gamma = 0.9, start = NULL, iters = 5000, tol = 1e-14)
}
\arguments{
\item{policy}{See Usage.}
\item{states}{See Usage.}
\item{actions}{See Usage.}
\item{step}{See Usage.}
\item{reward}{See Usage.}
\item{costs}{See Usage.}
\item{gamma}{Defaults to \code{0.9}.}
\item{start}{Defaults to \code{NULL}.}
\item{iters}{Defaults to \code{5000}.}
\item{tol}{Defaults to \code{1e-14}.}
}
\description{
Part of the safrl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{J}, \code{J_C}, \code{gamma}, \code{method}.
}
\keyword{internal}
