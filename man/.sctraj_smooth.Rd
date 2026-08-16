% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sctraj_smooth}
\alias{.sctraj_smooth}
\title{Local linear smoother of y on t -- the "smoothing spline" step,}
\usage{
.sctraj_smooth(t, y, w, span = 0.4)
}
\arguments{
\item{t}{A vector; its length is taken and its elements indexed.}
\item{y}{A vector; indexed elementwise.}
\item{w}{A vector; indexed elementwise.}
\item{span}{Numeric; combined arithmetically in the body. Defaults to \code{0.4}.}
}
\description{
kept simple and weight-aware.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.sctraj_smooth(t = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
