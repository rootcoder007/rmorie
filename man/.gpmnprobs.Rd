% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpmnprobs}
\alias{.gpmnprobs}
\title{Chapter 7: multinomial logistic model, eqs (7.6)-(7.10)}
\usage{
.gpmnprobs(X, beta0, beta, baseline_last = TRUE)
}
\arguments{
\item{X}{Passed to \code{.gpmat}.}
\item{beta0}{Passed to \code{.gpflat}.}
\item{beta}{Passed to \code{.gpmat}.}
\item{baseline_last}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
--------------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A matrix, from \code{t}.
}
\keyword{internal}
