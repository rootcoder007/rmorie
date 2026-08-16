% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.forsnp_locus}
\alias{.forsnp_locus}
\title{.forsnp_locus}
\usage{
.forsnp_locus(a1, a2, fr, theta)
}
\arguments{
\item{a1}{Passed to \code{==}.}
\item{a2}{Passed to \code{==}.}
\item{fr}{A vector; indexed elementwise.}
\item{theta}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the forsnp_native implementation. Called by \code{morie_forsnp}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.forsnp_locus(a1 = 5L, a2 = 5L, fr = c(1, 2, 3, 4, 5, 6, 7, 8), theta = 0.5)
}
