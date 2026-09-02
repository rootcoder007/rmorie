% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{policy_mean}
\alias{policy_mean}
\title{Mean outcome under a stochastic network-wide policy}
\usage{
policy_mean(Q_fn, W, friends, own_prob, seed = 0, draws = 200) 
}
\description{
Treatments are drawn independently with probability ``own_prob``, then the neighbourhood summary follows -- so the estimand is a property of the POLICY, which is the only thing well defined under interference.
}
\arguments{
\item{Q_fn}{Argument \code{Q_fn}; see Usage.}
\item{W}{Argument \code{W}; see Usage.}
\item{friends}{Argument \code{friends}; see Usage.}
\item{own_prob}{Argument \code{own_prob}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
\item{draws}{Argument \code{draws}; see Usage.}
}
\value{
A list with \code{psi}, \code{policy_prob}, \code{draws}, \code{N}.
}
\keyword{internal}
