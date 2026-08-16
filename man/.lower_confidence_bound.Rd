% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lower_confidence_bound}
\alias{.lower_confidence_bound}
\title{Lower confidence bound (Snoek 2012, Equation 3)}
\usage{
.lower_confidence_bound(mu, sd, kappa = 2)
}
\description{
Lower confidence bound (Snoek 2012, Equation 3)
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.lower_confidence_bound(V, V)
}
