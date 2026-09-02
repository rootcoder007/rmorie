% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{expected_features}
\alias{expected_features}
\title{expected_features}
\usage{
expected_features(n, alpha) 
}
\arguments{
\item{n}{Coerced to integer by the body, with \code{as.integer}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the ibpfa_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{expected_total_features}, \code{harmonic}, \code{expected_per_object}, \code{expected_nonzeros}, \code{note}.
}
\keyword{internal}
\examples{
expected_features(n = 5L, alpha = 0.5)
}
