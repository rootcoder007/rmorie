% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.shares}
\alias{.shares}
\title{.shares}
\usage{
.shares(protected, i)
}
\arguments{
\item{protected}{A vector; indexed elementwise.}
\item{i}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the fairRC_native implementation. Called by \code{.raw}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.shares(V, V)
}
