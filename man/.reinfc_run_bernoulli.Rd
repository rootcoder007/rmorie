% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reinfc_run_bernoulli}
\alias{.reinfc_run_bernoulli}
\title{.reinfc_run_bernoulli}
\usage{
.reinfc_run_bernoulli(reward_fn, pv, baseline, mode, rho, gamma, k, trials, rng)
}
\arguments{
\item{reward_fn}{Accepted by the signature and not used anywhere in the body.}
\item{pv}{A vector; its length is taken.}
\item{baseline}{Passed to \code{.reinfc_running_baseline}.}
\item{mode}{Compared against \code{"episodic"}.}
\item{rho}{Numeric; combined arithmetically in the body.}
\item{gamma}{Passed to \code{.reinfc_running_baseline}.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{trials}{A count; the body uses it as \code{seq_len(...)}.}
\item{rng}{Passed to \code{.ghc_unif}.}
}
\description{
A step of the reinfc_native implementation. Called by \code{morie_reinfc}. See the file header for the source the module follows.
}
\value{
The value of \code{.reinfc_finish}.
}
\keyword{internal}
