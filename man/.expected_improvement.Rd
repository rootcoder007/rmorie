% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.expected_improvement}
\alias{.expected_improvement}
\title{Expected improvement (Snoek 2012, Equation 2)}
\usage{
.expected_improvement(mu, sd, f_best, xi = 0)
}
\description{
Expected improvement (Snoek 2012, Equation 2)
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.expected_improvement(mu = c(1, 2, 3, 4, 5, 6, 7, 8), sd = c(1, 2, 3, 4, 5, 6, 7, 8), f_best = c(1, 2, 3, 4, 5, 6, 7, 8))
}
