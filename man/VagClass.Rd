% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VagClass}
\alias{VagClass}
\title{VagClass}
\usage{
VagClass(segments, durations = NULL, segclass = NULL, arthro = NULL)
}
\arguments{
\item{segments}{See Usage.}
\item{durations}{Defaults to \code{NULL}.}
\item{segclass}{Defaults to \code{NULL}.}
\item{arthro}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_class2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{varmeans}, \code{segmentmeans}, \code{abnormalfraction}, \code{normalfraction}, \code{decision}, \code{stage}, \code{abnormal}, \code{durations}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
VagClass(V)
}
