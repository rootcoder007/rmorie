% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pbinom}
\alias{Pbinom}
\title{P(X <= k) = I_{1-p}(n - k, k + 1)}
\usage{
Pbinom(q, size, prob, lower_tail = TRUE)
}
\arguments{
\item{q}{See Usage.}
\item{size}{See Usage.}
\item{prob}{See Usage.}
\item{lower_tail}{Defaults to \code{TRUE}.}
}
\description{
Part of the dist_native implementation; see the file header for the source it follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
Pbinom(q = 0.5, size = 5L, prob = 0.5)
}
