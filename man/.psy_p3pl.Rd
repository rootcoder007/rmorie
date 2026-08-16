% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.psy_p3pl}
\alias{.psy_p3pl}
\title{3PL item response function. c is the lower asymptote (guessing):}
\usage{
.psy_p3pl(theta, a, b, cc)
}
\arguments{
\item{theta}{A vector; its length is taken.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{A vector; its length is taken.}
\item{cc}{A count; the body uses it as \code{matrix(...)}.}
}
\description{
with c > 0 the curve never reaches zero.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.psy_p3pl(theta = c(1, 2, 3, 4, 5, 6, 7, 8), a = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L, cc = c(1, 2, 3, 4, 5, 6, 7, 8))
}
