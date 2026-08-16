% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.median}
\alias{.abcgp.median}
\title{.abcgp.median}
\usage{
.abcgp.median(v)
}
\arguments{
\item{v}{Numeric; passed to \code{sort}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.mw_sampler}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.abcgp.median(V)
}
