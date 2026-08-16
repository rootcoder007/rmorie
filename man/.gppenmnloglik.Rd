% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gppenmnloglik}
\alias{.gppenmnloglik}
\title{.gppenmnloglik}
\usage{
.gppenmnloglik(X, y, beta0, beta, lam, penalty = "ridge", baseline_last = TRUE)
}
\arguments{
\item{X}{Passed to \code{.gpmnloglik}.}
\item{y}{Passed to \code{.gpmnloglik}.}
\item{beta0}{Passed to \code{.gpmnloglik}.}
\item{beta}{Passed to \code{.gpmnloglik}.}
\item{lam}{See Usage.}
\item{penalty}{Defaults to \code{"ridge"}.}
\item{baseline_last}{Passed to \code{.gpmnloglik}. Defaults to \code{TRUE}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm109}, \code{Msm115}. See the file header for the source the module follows.
}
\value{
A list with \code{loglik}, \code{penalty}, \code{penalized_loglik}.
}
\keyword{internal}
