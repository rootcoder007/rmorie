% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{RunIntAll}
\alias{RunIntAll}
\title{Eq (3.113).  Over a finite record the lower limit is the first}
\usage{
RunIntAll(x, t) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
sample, so any mass before it is unobserved and the constant of integration is arbitrary.  The discrete counterpart has a pole ON the unit circle at DC, which is why the book says it is seldom used for filtering.
}
\value{
A list with \code{y}, \code{n}, \code{total}, \code{lower_limit}, \code{constant_of_integration_is_arbitrary}, \code{discrete_pole_on_the_unit_circle}, \code{seldom_used_for_filtering}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
RunIntAll(V, V)
}
