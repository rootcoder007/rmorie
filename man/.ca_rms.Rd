% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ca_rms}
\alias{.ca_rms}
\title{.ca_rms}
\usage{
.ca_rms(a, b)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
Part of the caCMIP_mixedcase_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ca_rms(V, V)
}
