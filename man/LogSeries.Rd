% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LogSeries}
\alias{LogSeries}
\title{Eq (4.69): log(1 + x) = x - x^2/2 + x^3/3 - ..., |x| < 1.  The radius}
\usage{
LogSeries(x, terms = 20)
}
\arguments{
\item{x}{Coerced to complex by the body, with \code{as.complex}.}
\item{terms}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.}
}
\description{
is exactly 1, so |x| >= 1 is refused instead of diverging quietly.
}
\value{
A list with \code{value}, \code{exact}, \code{error}, \code{error_bound}, \code{terms}, \code{method}.
}
\keyword{internal}
