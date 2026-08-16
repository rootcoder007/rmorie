% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MotionArt}
\alias{MotionArt}
\title{Section 1.2.11 characterises motion artifact but gives no detection}
\usage{
MotionArt(x, fs, win = 1, factor = 4)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{win}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{factor}{Numeric; combined arithmetically in the body. Defaults to \code{4}.}
}
\description{
equation, so none is attributed to the book.  Window range and window activity are both compared against their MEDIANS, because the mean is dragged upward by the very segments being detected.
}
\value{
A list with \code{clean}, \code{artifact}, \code{nsegments}, \code{fraction}, \code{win}, \code{factor}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(7)
fs <- 100
x <- c(rnorm(fs * 2, 0, 0.5), rnorm(fs, 0, 4), rnorm(fs * 2, 0, 0.5))
MotionArt(x, fs = fs)
}
