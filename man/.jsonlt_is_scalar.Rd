% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_is_scalar}
\alias{.jsonlt_is_scalar}
\title{.jsonlt_is_scalar}
\usage{
.jsonlt_is_scalar(x)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
A step of the jsonlt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A logical value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.jsonlt_is_scalar(V)
}
