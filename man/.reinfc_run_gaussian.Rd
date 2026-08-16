% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.reinfc_run_gaussian}
\alias{.reinfc_run_gaussian}
\title{.reinfc_run_gaussian}
\usage{
.reinfc_run_gaussian(reward_fn, mu, sigma, baseline, mode, alpha, gamma, k, trials, rate_scaling, rng)
}
\arguments{
\item{reward_fn}{Accepted by the signature and not used anywhere in the body.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{sigma}{Numeric; combined arithmetically in the body.}
\item{baseline}{Passed to \code{.reinfc_running_baseline}.}
\item{mode}{Compared against \code{"episodic"}.}
\item{alpha}{Numeric; combined arithmetically in the body.}
\item{gamma}{Passed to \code{.reinfc_running_baseline}.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
\item{trials}{A count; the body uses it as \code{seq_len(...)}.}
\item{rate_scaling}{One of \code{"none"}, \code{"sigma2"}.}
\item{rng}{Passed to \code{.ghc_unif}.}
}
\description{
A step of the reinfc_native implementation. Called by \code{morie_reinfc}. See the file header for the source the module follows.
}
\value{
The value of \code{result}, as built in the body.
}
\keyword{internal}
