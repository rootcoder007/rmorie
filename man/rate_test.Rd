% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{rate_test}
\alias{rate_test}
\title{rate_test}
\usage{
rate_test(scores, priority, weight = "autoc", reps = 500, seed = 0)
}
\arguments{
\item{scores}{See Usage.}
\item{priority}{See Usage.}
\item{weight}{Defaults to \code{"autoc"}.}
\item{reps}{Defaults to \code{500}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the slvgrf_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{z}, \code{p_value}, \code{weight}, \code{reps}, \code{n}, \code{null}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rate_test(V, V)
}
