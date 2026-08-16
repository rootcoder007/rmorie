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
\item{alpha}{See Usage.}
\item{tol}{Defaults to \code{0.001}.}
}
\description{
Part of the slowdp_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{K}, \code{expected_tail}, \code{tolerance}, \code{note}.
}
\keyword{internal}
\examples{
sticks_for_tolerance(alpha = 0.5)
}
