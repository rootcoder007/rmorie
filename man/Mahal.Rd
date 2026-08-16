% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Mahal}
\alias{Mahal}
\title{Section 10.4.3: D^2 = (x - mu)^T C^-1 (x - mu).  Distance in units of}
\usage{
Mahal(x, mu, C)
}
\arguments{
\item{x}{See Usage.}
\item{mu}{See Usage.}
\item{C}{See Usage.}
}
\description{
the data\\'s own scatter: a point far along an axis of natural variation is NEAR, one close by across the grain is far.  Euclidean distance would quietly favour whichever feature has the largest units.
}
\value{
A list with \code{d2}, \code{distance}, \code{squared}, \code{euclidean}, \code{differs_from_euclidean}, \code{scale_free}, \code{method}.
}
\keyword{internal}
