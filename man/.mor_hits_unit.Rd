% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_hits_unit}
\alias{.mor_hits_unit}
\title{.mor_hits_unit}
\usage{
.mor_hits_unit(v)
}
\arguments{
\item{v}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the hits_native implementation. Called by \code{morie_hits}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_hits_unit(V)
}
