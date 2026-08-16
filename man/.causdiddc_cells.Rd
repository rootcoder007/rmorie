% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causdiddc_cells}
\alias{.causdiddc_cells}
\title{Collapse to (group, period) cells: mean Y, count, treatment}
\usage{
.causdiddc_cells(Y, D, g, t)
}
\arguments{
\item{Y}{A vector; indexed elementwise.}
\item{D}{A vector; indexed elementwise.}
\item{g}{Passed to \code{paste0}.}
\item{t}{Passed to \code{paste0}.}
}
\description{
A step of the causdiddc_native implementation. Called by \code{.causdiddc_did_m}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.causdiddc_cells(Y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), g = c(1, 2, 3, 4, 5, 6, 7, 8), t = c(1, 2, 3, 4, 5, 6, 7, 8))
}
