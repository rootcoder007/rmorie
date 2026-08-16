% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LmsSqErr}
\alias{LmsSqErr}
\title{Eq. (3.200): e^2 = x^2 - 2 x r\\'w + (r\\'w)^2.  The expansion is}
\usage{
LmsSqErr(x, rvec, w)
}
\arguments{
\item{x}{See Usage.}
\item{rvec}{See Usage.}
\item{w}{See Usage.}
}
\description{
checked against the square itself.  This is the INSTANTANEOUS squared error standing in for the expectation -- that substitution is the whole of LMS, and the reason the algorithm converges only in the mean.
}
\value{
A list with \code{e}, \code{e_squared}, \code{expanded}, \code{max_difference}, \code{agrees}, \code{nonnegative}, \code{instantaneous_not_expected}, \code{method}.
}
\keyword{internal}
\examples{
LmsSqErr(x = c(1, 2, 3, 4, 5, 6, 7, 8), rvec = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
