% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.merge_height}
\alias{.merge_height}
\title{.merge_height}
\usage{
.merge_height(s_left, s_mid, s_right, h_left, h_right)
}
\arguments{
\item{s_left}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{s_mid}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{s_right}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h_left}{Numeric; passed to \code{log}.}
\item{h_right}{Numeric; passed to \code{log}.}
}
\description{
A step of the bayrjmcmc_native implementation. Called by \code{changepoint_rjmcmc}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.merge_height(s_left = c(1, 2, 3, 4, 5, 6, 7, 8), s_mid = c(1, 2, 3, 4, 5, 6, 7, 8), s_right = c(1, 2, 3, 4, 5, 6, 7, 8), h_left = c(1, 2, 3, 4, 5, 6, 7, 8), h_right = c(1, 2, 3, 4, 5, 6, 7, 8))
}
