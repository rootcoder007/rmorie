% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vepan_find}
\alias{.vepan_find}
\title{0-based index of ch in s, or -1 (like Python str.find)}
\usage{
.vepan_find(s, ch)
}
\arguments{
\item{s}{Passed to \code{regexpr}.}
\item{ch}{Passed to \code{regexpr}.}
}
\description{
A step of the vepan_native implementation. Called by \code{.vepan_coding_terms}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.vepan_find(s = 5L, ch = c(1, 2, 3, 4, 5, 6, 7, 8))
}
