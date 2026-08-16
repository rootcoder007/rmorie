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
\item{lam}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{penalty}{Passed to \code{identical}. Defaults to \code{"ridge"}.}
\item{baseline_last}{Passed to \code{.gpmnloglik}. Defaults to \code{TRUE}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{Msm109}, \code{Msm115}. See the file header for the source the module follows.
}
\value{
A list with \code{loglik}, \code{penalty}, \code{penalized_loglik}.
}
\keyword{internal}
\examples{
.gppenmnloglik(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = 5L, beta0 = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5, lam = c(1, 2, 3, 4, 5, 6, 7, 8))
}
