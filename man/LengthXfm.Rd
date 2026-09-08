% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LengthXfm}
\alias{LengthXfm}
\title{Eq (4.21) (Gritzali et al.).  Since (dx/dt) dt = dx, the transform is}
\usage{
LengthXfm(chans, wwin, fs) 
}
\arguments{
\item{chans}{A vector; its length is taken.}
\item{wwin}{Numeric; combined arithmetically in the body.}
\item{fs}{Numeric; combined arithmetically in the body.}
}
\description{
the arc length of the MULTICHANNEL trajectory accumulated over a w-second window.  Summing the squared derivative across channels before integrating is the whole point: a wave well defined in only one lead still contributes.
}
\value{
A list with \code{length}, \code{nchan}, \code{wsamp}, \code{wsec}, \code{fs}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(7)
fs <- 200
n <- fs * 2
x1 <- rep(0, n)
for (b in seq(0.4, 1.8, by = 0.7)) {
  i <- round(b * fs); x1[i:(i + 4)] <- c(0.3, 1.2, -0.4, 0.1, 0)
}
x2 <- 0.5 * x1 + 0.01 * rnorm(n)
r <- LengthXfm(list(x1, x2), wwin = 0.12, fs = fs)
str(r, max.level = 1)
}
