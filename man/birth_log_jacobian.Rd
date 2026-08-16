% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{birth_log_jacobian}
\alias{birth_log_jacobian}
\title{birth_log_jacobian}
\usage{
birth_log_jacobian(h_j, h_new_left, h_new_right)
}
\arguments{
\item{h_j}{See Usage.}
\item{h_new_left}{See Usage.}
\item{h_new_right}{See Usage.}
}
\description{
A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
birth_log_jacobian(h_j = c(1, 2, 3, 4, 5, 6, 7, 8), h_new_left = c(1, 2, 3, 4, 5, 6, 7, 8), h_new_right = c(1, 2, 3, 4, 5, 6, 7, 8))
}
