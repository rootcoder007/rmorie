% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qt}
\alias{Qt}
\title{The t is symmetric: qt(p) = -qt(1 - p) and qt(0.5) = 0 exactly}
\usage{
Qt(p, df)
}
\arguments{
\item{p}{Iterated over elementwise, with \code{vapply}.}
\item{df}{Passed to \code{Pt}.}
}
\description{
Without this, bisection lands on the cdf plateau around zero -- betainc\'s 1 - xb underflows for |v| < ~1e-8, the cdf sits at exactly 0.5 there, and both bisection and Newton are blind inside it.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Qt(V, V)
}
