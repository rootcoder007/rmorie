% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{decay_diagnostics}
\alias{decay_diagnostics}
\title{decay_diagnostics}
\usage{
decay_diagnostics(weights, alpha)
}
\arguments{
\item{weights}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{alpha}{See Usage.}
}
\description{
A step of the slowdp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{realised_tail}, \code{expected_tail}, \code{ratio}, \code{largest_index}, \code{monotone}, \code{note}.
}
\keyword{internal}
\examples{
decay_diagnostics(weights = c(1, 2, 3, 4, 5, 6, 7, 8), alpha = 0.5)
}
