% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.log_k_prior_ratio}
\alias{.log_k_prior_ratio}
\title{.log_k_prior_ratio}
\usage{
.log_k_prior_ratio(lam, k)
}
\arguments{
\item{lam}{Numeric; passed to \code{log}.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.log_k_prior_ratio(V, V)
}
