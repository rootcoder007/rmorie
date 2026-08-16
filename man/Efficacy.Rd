% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Efficacy}
\alias{Efficacy}
\title{Efficacy -- eq. (13.2.4), p. 486}
\usage{
Efficacy(deriv, var)
}
\description{
Efficacy -- eq. (13.2.4), p. 486
}
\value{
A list with \code{efficacy}, \code{deriv}, \code{var}.
}
\keyword{internal}
\examples{
Efficacy(deriv = c(1, 2, 3, 4, 5, 6, 7, 8), var = 5L)
}
