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
\item{R0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{si_mean}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{si_sd}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{delay_mean}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{delay_sd}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{trace_prob}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{subclinical}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{draws}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{20000}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the ttrace_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{R_eff}, \code{R0}, \code{fraction_before_isolation}, \code{controlled_in_expectation}, \code{note}.
}
\keyword{internal}
