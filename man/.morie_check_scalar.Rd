% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_check_scalar}
\alias{.morie_check_scalar}
\title{Internal helper: Morie Check Scalar}
\usage{
.morie_check_scalar(x, type = c("numeric", "character", "logical", "integer"), arg = "x")
}
\description{
Internal helper: Morie Check Scalar
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
.morie_check_scalar(x = 5L)
}
