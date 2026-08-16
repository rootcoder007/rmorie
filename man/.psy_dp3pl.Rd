% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.psy_dp3pl}
\alias{.psy_dp3pl}
\title{.psy_dp3pl}
\usage{
.psy_dp3pl(theta, a, b, cc)
}
\arguments{
\item{theta}{Passed to \code{.psy_p3pl}.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Passed to \code{.psy_p3pl}.}
\item{cc}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the psycho_native implementation. Called by \code{.psy_info}. See the file header for the source the module follows.
}
\value{
The value of \code{sweep}.
}
\keyword{internal}
\examples{
.psy_dp3pl(theta = c(1, 2, 3, 4, 5, 6, 7, 8), a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, cc = c(1, 2, 3, 4, 5, 6, 7, 8))
}
