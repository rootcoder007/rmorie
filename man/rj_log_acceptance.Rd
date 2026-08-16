% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rj_log_acceptance}
\alias{rj_log_acceptance}
\title{rj_log_acceptance}
\usage{
rj_log_acceptance(logpost_from, logpost_to, log_j_from, log_j_to, logq_u, logq_u_rev, log_jacobian)
}
\arguments{
\item{logpost_from}{Numeric; combined arithmetically in the body.}
\item{logpost_to}{Numeric; combined arithmetically in the body.}
\item{log_j_from}{Numeric; combined arithmetically in the body.}
\item{log_j_to}{Numeric; combined arithmetically in the body.}
\item{logq_u}{Numeric; combined arithmetically in the body.}
\item{logq_u_rev}{Numeric; combined arithmetically in the body.}
\item{log_jacobian}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bayrjmcmc_native implementation. Called by \code{reversible_jump_mcmc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
