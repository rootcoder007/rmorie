% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.masrcn_bilinear}
\alias{.masrcn_bilinear}
\title{.masrcn_bilinear}
\usage{
.masrcn_bilinear(F, y, x)
}
\arguments{
\item{F}{A vector; its length is taken and its elements indexed.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{x}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the masrcn_native implementation. Called by \code{roi_align}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.masrcn_bilinear(F = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
