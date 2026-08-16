% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rmrl_qlearn_flat}
\alias{morie_rmrl_qlearn_flat}
\title{morie_rmrl_qlearn_flat}
\usage{
morie_rmrl_qlearn_flat(machine, states, actions, step, label, gamma = 0.9, alpha = 0.5, epsilon = 0.1, episodes = 500, horizon = 100, start = NULL, dead_end = NULL, seed = 0)
}
\arguments{
\item{machine}{A list; the body reads \code{$states}, \code{$terminal}, \code{$u0} from it.}
\item{states}{See Usage.}
\item{actions}{See Usage.}
\item{step}{See Usage.}
\item{label}{See Usage.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0.9}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{epsilon}{Passed to \code{.rmrl_eps_greedy}. Defaults to \code{0.1}.}
\item{episodes}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{500}.}
\item{horizon}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100}.}
\item{start}{Defaults to \code{NULL}.}
\item{dead_end}{Defaults to \code{NULL}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
}
\description{
A step of the rmrl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{returns}, \code{mean_return_last}, \code{mean_return_first}, \code{method}.
}
\keyword{internal}
