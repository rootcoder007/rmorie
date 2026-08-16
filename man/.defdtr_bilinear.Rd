% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.defdtr_bilinear}
\alias{.defdtr_bilinear}
\title{Bilinear sample of a feature map at a real-valued location}
\usage{
.defdtr_bilinear(F, y, x)
}
\arguments{
\item{F}{A matrix; indexed by row and column.}
\item{y}{Numeric; passed to \code{max}.}
\item{x}{Numeric; passed to \code{max}.}
}
\description{
A step of the defdtr implementation. Called by \code{Defdtr}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.defdtr_bilinear(F = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), y = c(1, 2, 3, 4, 5, 6, 7, 8), x = c(1, 2, 3, 4, 5, 6, 7, 8))
}
