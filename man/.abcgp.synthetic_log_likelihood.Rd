% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.synthetic_log_likelihood}
\alias{.abcgp.synthetic_log_likelihood}
\title{.abcgp.synthetic_log_likelihood}
\usage{
.abcgp.synthetic_log_likelihood(draws, obs, epsilon = 0, summary = NULL)
}
\arguments{
\item{draws}{See Usage.}
\item{obs}{Passed to \code{.abcgp.summarise}.}
\item{epsilon}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{summary}{Passed to \code{.abcgp.summarise}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}. See the file header for the source the module follows.
}
\value{
A list with \code{log_lik}, \code{mu}, \code{cov}.
}
\keyword{internal}
