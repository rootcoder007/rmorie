% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.raw}
\alias{.raw}
\title{.raw}
\usage{
.raw(protected, measure, step)
}
\arguments{
\item{protected}{A vector; its length is taken and its elements indexed.}
\item{measure}{One of \code{"rKL"}, \code{"rND"}.}
\item{step}{See Usage.}
}
\description{
A step of the fairRC_native implementation. Called by \code{.measure}, \code{normalizer}. See the file header for the source the module follows.
}
\value{
The value of \code{tot}, as built in the body.
}
\keyword{internal}
