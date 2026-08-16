% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.probability_of_improvement}
\alias{.probability_of_improvement}
\title{Probability of improvement (Snoek 2012, Equation 1)}
\usage{
.probability_of_improvement(mu, sd, f_best, xi = 0)
}
\description{
Probability of improvement (Snoek 2012, Equation 1)
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.probability_of_improvement(mu = c(1, 2, 3, 4, 5, 6, 7, 8), sd = c(1, 2, 3, 4, 5, 6, 7, 8), f_best = c(1, 2, 3, 4, 5, 6, 7, 8))
}
