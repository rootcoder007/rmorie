% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpepe}
\alias{.gpepe}
\title{Chapter 3: expected prediction error, p.80}
\usage{
.gpepe(sigma2, x_star, eigenvalues)
}
\arguments{
\item{sigma2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x_star}{Passed to \code{.gpflat}.}
\item{eigenvalues}{Passed to \code{.gpflat}.}
}
\description{
--------------------------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.gpepe(sigma2 = c(1, 2, 3, 4, 5, 6, 7, 8), x_star = c(1, 2, 3, 4, 5, 6, 7, 8), eigenvalues = c(1, 2, 3, 4, 5, 6, 7, 8))
}
