% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{effective_reproduction_number}
\alias{effective_reproduction_number}
\title{effective_reproduction_number}
\usage{
effective_reproduction_number(R0, si_mean, si_sd, delay_mean, delay_sd, trace_prob, subclinical = 0, draws = 20000, seed = 0)
}
\arguments{
\item{R0}{See Usage.}
\item{si_mean}{See Usage.}
\item{si_sd}{See Usage.}
\item{delay_mean}{See Usage.}
\item{delay_sd}{See Usage.}
\item{trace_prob}{See Usage.}
\item{subclinical}{Defaults to \code{0}.}
\item{draws}{Defaults to \code{20000}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the ttrace_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{R_eff}, \code{R0}, \code{fraction_before_isolation}, \code{controlled_in_expectation}, \code{note}.
}
\keyword{internal}
