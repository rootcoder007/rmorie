% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TWaveDet}
\alias{TWaveDet}
\title{Gritzali et al., Section 4.3.4.  The QRS is detected first, blanked}
\usage{
TWaveDet(chans, qrs, fs, tdur = 0.16)
}
\arguments{
\item{chans}{A vector; its length is taken.}
\item{qrs}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{tdur}{Numeric; combined arithmetically in the body. Defaults to \code{0.16}.}
}
\description{
to the isoelectric baseline, and the length transform of eq (4.21) re-run with the window at the average T duration.  Working ACROSS channels is what makes T detection tractable.
}
\value{
A list with \code{t}, \code{onset}, \code{offset}, \code{length}, \code{tdur}, \code{nchan}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(5)
fs <- 250
n <- fs * 3
x <- rep(0, n)
qrs <- round(seq(0.5, 2.7, by = 0.75) * fs)
for (i in qrs) {
  x[i:(i + 5)] <- c(0.2, 1.2, -0.5, 0.1, 0.05, 0)
  j <- i + round(0.25 * fs)
  x[j:(j + 12)] <- 0.3 * exp(-((0:12 - 6) / 3)^2)
}
x <- x + 0.005 * rnorm(n)
r <- TWaveDet(list(x), qrs = qrs, fs = fs)
str(r, max.level = 1)
}
