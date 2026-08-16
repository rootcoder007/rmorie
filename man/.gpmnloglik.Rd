% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpmnloglik}
\alias{.gpmnloglik}
\title{.gpmnloglik}
\usage{
.gpmnloglik(X, y, beta0, beta, baseline_last = TRUE)
}
\arguments{
\item{X}{Passed to \code{.gpmnprobs}.}
\item{y}{Passed to \code{.gpflat}.}
\item{beta0}{Passed to \code{.gpmnprobs}.}
\item{beta}{Passed to \code{.gpmnprobs}.}
\item{baseline_last}{Passed to \code{.gpmnprobs}. Defaults to \code{TRUE}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{.gppenmnloglik}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
