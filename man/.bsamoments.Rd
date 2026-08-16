% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsamoments}
\alias{.bsamoments}
\title{(mean, variance, skewness, kurtosis); kurtosis is the raw fourth}
\usage{
.bsamoments(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
standardised moment (3.0 for a Gaussian).
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bsamoments(V)
}
