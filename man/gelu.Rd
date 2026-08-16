% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gelu}
\alias{gelu}
\title{GELU exact using erf; avoid pnorm dependency for portability}
\usage{
gelu(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
Part of the berte_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gelu(V)
}
