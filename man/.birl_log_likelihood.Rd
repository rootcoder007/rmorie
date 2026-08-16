% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.birl_log_likelihood}
\alias{.birl_log_likelihood}
\title{.birl_log_likelihood}
\usage{
.birl_log_likelihood(Q, observations, alpha = 1)
}
\arguments{
\item{Q}{A matrix; indexed by row and column.}
\item{observations}{A vector; its length is taken.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the birl_native implementation. Called by \code{policy_walk}. See the file header for the source the module follows.
}
\value{
The value of \code{total}, as built in the body.
}
\keyword{internal}
