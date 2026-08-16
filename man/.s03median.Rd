% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03median}
\alias{.s03median}
\title{.s03median}
\usage{
.s03median(v)
}
\arguments{
\item{v}{Numeric; passed to \code{sort}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.dnnheadweights}, \code{.s03mad}, \code{Epicur} and 4 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03median(V)
}
