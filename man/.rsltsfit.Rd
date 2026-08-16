% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsltsfit}
\alias{.rsltsfit}
\title{Ordinary least squares on a subset, by the normal equations}
\usage{
.rsltsfit(Xm, yy, idx, p)
}
\arguments{
\item{Xm}{A matrix; indexed by row and column.}
\item{yy}{A vector; indexed elementwise.}
\item{idx}{See Usage.}
\item{p}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the helpers_rouss implementation. Called by \code{Ltsreg}. See the file header for the source the module follows.
}
\value{
The value of \code{.rslusolve}.
}
\keyword{internal}
