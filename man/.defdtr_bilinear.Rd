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
