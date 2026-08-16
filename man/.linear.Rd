% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.linear}
\alias{.linear}
\title{.linear}
\usage{
.linear(x, Wm, b)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{Wm}{A matrix; indexed by row and column.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the mambss_native implementation. Called by \code{selective_scan}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
