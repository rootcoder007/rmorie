% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Laplace}
\alias{Laplace}
\title{Eq (3.50), by the trapezoidal rule over the samples supplied.  What}
\usage{
Laplace(h, t, s) 
}
\arguments{
\item{h}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{s}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
is returned is the transform OF THE SAMPLED RECORD over the interval it covers, which is why the limits come back with it.
}
\value{
A list with \code{H}, \code{s}, \code{t_min}, \code{t_max}, \code{n}, \code{trapezoidal}, \code{over_the_sampled_interval_only}, \code{method}.
}
\keyword{internal}
\examples{
Laplace(h = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8), s = c(1, 2, 3, 4, 5, 6, 7, 8))
}
