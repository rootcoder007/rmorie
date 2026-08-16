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
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{beta0}{See Usage.}
\item{beta}{See Usage.}
\item{lam}{See Usage.}
\item{penalty}{Defaults to \code{"ridge"}.}
\item{baseline_last}{Defaults to \code{TRUE}.}
}
\description{
Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A list with \code{loglik}, \code{penalty}, \code{penalized_loglik}.
}
\keyword{internal}
