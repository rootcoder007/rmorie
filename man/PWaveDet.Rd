% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PWaveDet}
\alias{PWaveDet}
\title{Hengeveld and van Bemmel, Section 4.3.3.  The P wave is never}
\usage{
PWaveDet(x, qrs, fs, template = NULL) 
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{qrs}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{template}{Optional; may be \code{NULL}. A vector; its length is taken.}
}
\description{
searched for directly: the QRS is deleted, the residue bandpassed 3-11 Hz, ternarised at 50\% and 75\% of the interval maximum, and matched against a ternary template.  Matching a TERNARY version is what makes it tolerant of P-wave amplitude and shape variation.
}
\value{
A list with \code{p}, \code{template}, \code{windows}, \code{bandpass}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(4)
fs <- 250
n <- fs * 3
x <- rep(0, n)
qrs <- round(seq(0.5, 2.7, by = 0.75) * fs)
for (i in qrs) {
  x[(i - round(0.15 * fs)):(i - round(0.15 * fs) + 8)] <-
    0.15 * exp(-((0:8 - 4) / 2)^2)
  x[i:(i + 5)] <- c(0.2, 1.2, -0.5, 0.1, 0.05, 0)
}
x <- x + 0.005 * rnorm(n)
r <- PWaveDet(x, qrs = qrs, fs = fs)
str(r, max.level = 1)
}
