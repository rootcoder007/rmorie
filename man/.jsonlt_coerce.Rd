% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_coerce}
\alias{.jsonlt_coerce}
\title{.jsonlt_coerce}
\usage{
.jsonlt_coerce(v, t)
}
\arguments{
\item{v}{Optional; may be \code{NULL}. One of \code{"-Inf"}, \code{"Inf"}, \code{"NaN"}.}
\item{t}{One of \code{"complex"}, \code{"double"}, \code{"integer"}, \code{"logical"}.}
}
\description{
A step of the jsonlt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A character value.
}
\keyword{internal}
\examples{
.jsonlt_coerce(v = c(1, 2, 3, 4, 5, 6, 7, 8), t = 5L)
}
