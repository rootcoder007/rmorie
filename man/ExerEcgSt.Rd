% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ExerEcgSt}
\alias{ExerEcgSt}
\title{ST level at J + jofs relative to the PQ isoelectric level, and the}
\usage{
ExerEcgSt(x, qrs, fs, jofs = 0.06, thresh = 0.1)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{qrs}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{jofs}{Numeric; combined arithmetically in the body. Defaults to \code{0.06}.}
\item{thresh}{Carried through into a list the body builds. Defaults to \code{0.1}.}
}
\description{
slope of the segment that follows (Section 1.2.4).  The 0.1 mV threshold is the conventional clinical figure, NOT a book value -- the payload says so.
}
\value{
A list with \code{stdev}, \code{stslope}, \code{pattern}, \code{stdevmean}, \code{stslopemean}, \code{flagged}, \code{thresh}, \code{threshnote}, \code{jofs}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(2)
fs <- 250
n <- fs * 3
x <- rep(0, n)
qrs <- round(seq(0.5, 2.7, by = 0.75) * fs)
for (i in qrs) x[i:(i + 5)] <- c(0.2, 1.1, -0.5, 0.05, 0.02, 0)
x <- x + 0.01 * rnorm(n)
r <- ExerEcgSt(x, qrs = qrs, fs = fs)
str(r, max.level = 1)
}
