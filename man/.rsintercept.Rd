% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rsintercept}
\alias{.rsintercept}
\title{Index of the first all-ones design column, or 0 when there is none}
\usage{
.rsintercept(Xm, n, p)
}
\arguments{
\item{Xm}{A matrix; indexed by row and column.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{p}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the helpers_rouss implementation. Called by \code{Lmsreg}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
