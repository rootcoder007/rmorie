% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{optimal_policy}
\alias{optimal_policy}
\title{optimal_policy}
\usage{
optimal_policy(logp_ref, reward, beta) 
}
\arguments{
\item{logp_ref}{Passed to \code{.dpoF_vec}.}
\item{reward}{Passed to \code{.dpoF_vec}.}
\item{beta}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the dpoF_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{list}.
}
\keyword{internal}
\examples{
optimal_policy(logp_ref = c(1, 2, 3, 4, 5, 6, 7, 8), reward = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5)
}
