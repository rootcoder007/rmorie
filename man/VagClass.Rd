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
\item{segments}{Passed to \code{.morie_bx_mat}.}
\item{durations}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_vec}.}
\item{segclass}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_vec}.}
\item{arthro}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_vec}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{varmeans}, \code{segmentmeans}, \code{abnormalfraction}, \code{normalfraction}, \code{decision}, \code{stage}, \code{abnormal}, \code{durations}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
VagClass(V)
}
