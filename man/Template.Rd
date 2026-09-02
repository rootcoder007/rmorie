% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Template}
\alias{Template}
\title{Eqs (4.25), (4.28): the correlation coefficient at every shift}
\usage{
Template(x, ref, threshold = NULL, subtract_mean = TRUE) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{ref}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{threshold}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{subtract_mean}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
gamma is normalized at EVERY shift by the energy of the segment under the template, which is what makes it a correlation rather than a convolution -- a large low-frequency excursion cannot produce a spurious match because the normalization divides the amplitude out.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Template(V, V)
}
