% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.smcsam_pick}
\alias{.smcsam_pick}
\title{Binary search over a precomputed cumulative distribution.  R\\'s}
\usage{
.smcsam_pick(cum, u)
}
\arguments{
\item{cum}{A vector; its length is taken and its elements indexed.}
\item{u}{See Usage.}
}
\description{
findInterval has the same effect but is documented to return the leftmost interval, so we keep our own to mirror the Python arm\\'s hand-written loop.
}
\value{
The value of \code{lo}, as built in the body.
}
\keyword{internal}
