% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reinfc_run_logistic}
\alias{.reinfc_run_logistic}
\title{.reinfc_run_logistic}
\usage{
.reinfc_run_logistic(reward_fn, xs, wm, baseline, mode, alpha, gamma, k, trials, eligibility, rng)
}
\arguments{
\item{reward_fn}{Accepted by the signature and not used anywhere in the body.}
\item{xs}{A matrix; indexed by row and column.}
\item{wm}{A matrix; passed to \code{nrow}.}
\item{baseline}{Passed to \code{.reinfc_running_baseline}.}
\item{mode}{Compared against \code{"episodic"}.}
\item{alpha}{Numeric; combined arithmetically in the body.}
\item{gamma}{Numeric; combined arithmetically in the body.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{trials}{A count; the body uses it as \code{seq_len(...)}.}
\item{eligibility}{Compared against \code{"ybar"}.}
\item{rng}{Passed to \code{.ghc_unif}.}
}
\description{
A step of the reinfc_native implementation. Called by \code{morie_reinfc}. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
