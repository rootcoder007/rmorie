% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rate}
\alias{rate}
\title{rate}
\usage{
rate(scores, priority, weight = "autoc") 
}
\arguments{
\item{scores}{Passed to \code{toc_curve}.}
\item{priority}{Passed to \code{toc_curve}.}
\item{weight}{Compared against \code{"qini"}. Defaults to \code{"autoc"}.}
}
\description{
A step of the slvgrf_native implementation. Called by \code{autoc}, \code{qini_coefficient}, \code{rate_test}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{weight}, \code{curve}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rate(V, V)
}
