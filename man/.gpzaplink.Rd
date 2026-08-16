% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpzaplink}
\alias{.gpzaplink}
\title{Chapter 15: zero-altered Poisson forest, eqs (15.1)-(15.4)}
\usage{
.gpzaplink(mu_pred, theta_pred)
}
\arguments{
\item{mu_pred}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{theta_pred}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
----------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A list with \code{mu}, \code{theta}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gpzaplink(V, V)
}
