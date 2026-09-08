% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sticks_for_tolerance}
\alias{sticks_for_tolerance}
\title{sticks_for_tolerance}
\usage{
sticks_for_tolerance(alpha, tol = 0.001) 
}
\arguments{
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.001}.}
}
\description{
A step of the slowdp_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{K}, \code{expected_tail}, \code{tolerance}, \code{note}.
}
\keyword{internal}
\examples{
sticks_for_tolerance(alpha = 0.5)
}
