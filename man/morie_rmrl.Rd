% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_rmrl}
\alias{morie_rmrl}
\title{morie_rmrl}
\usage{
morie_rmrl(machines, states, actions, step, label, gamma = 0.9, alpha = 0.5, epsilon = 0.1, episodes = 500, horizon = 100, start = NULL, dead_end = NULL, seed = 0, task_order = NULL)
}
\arguments{
\item{machines}{A vector; its length is taken and its elements indexed.}
\item{states}{Coerced to list by the body, with \code{as.list}.}
\item{actions}{Coerced to list by the body, with \code{as.list}.}
\item{step}{A function; the body checks with \code{is.function}.}
\item{label}{A function; the body checks with \code{is.function}.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0.9}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
\item{epsilon}{Passed to \code{.rmrl_eps_greedy}. Defaults to \code{0.1}.}
\item{episodes}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{500}.}
\item{horizon}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100}.}
\item{start}{Optional; may be \code{NULL}. A function; the body checks with \code{is.function}.}
\item{dead_end}{A function; the body checks with \code{is.function}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{task_order}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rmrl_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{q}, \code{policy}, \code{returns}, \code{mean_return_last}, \code{mean_return_first}, \code{n_qfunctions}, \code{episodes}, \code{method}.
}
\keyword{internal}
