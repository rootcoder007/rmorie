% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{toc_curve}
\alias{toc_curve}
\title{toc_curve}
\usage{
toc_curve(scores, priority) 
}
\arguments{
\item{scores}{Passed to \code{.slvgrf_check}.}
\item{priority}{Passed to \code{.slvgrf_check}.}
}
\description{
A step of the slvgrf_native implementation. Called by \code{rate}. See the file header for the source the module follows.
}
\value{
A list with \code{u}, \code{toc}, \code{ate}, \code{order}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
toc_curve(V, V)
}
