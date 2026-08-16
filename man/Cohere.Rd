% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Cohere}
\alias{Cohere}
\title{Eq (4.32).  The book is emphatic: computed from two single}
\usage{
Cohere(x, y, fs = 1, nperseg = NULL, noverlap = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{nperseg}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{noverlap}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
observations the magnitude is UNITY AT EVERY FREQUENCY, which is incorrect; each density must be estimated by AVERAGING over several observations.  So a single segment is refused rather than returning the meaningless all-ones answer.
}
\value{
A list with \code{coherence}, \code{phase}, \code{sxx}, \code{syy}, \code{sxy}, \code{freqs}, \code{n_segments}, \code{nperseg}, \code{method}.
}
\keyword{internal}
\examples{
sine_c <- function(n, cycles, amp = 1, phase = 0) amp * sin(2 * 
    pi * cycles * (0:(n - 1))/n + phase)
r <- DotProd(c(1, 2, 3), c(4, 5, 6))
n <- 1024
x <- sine_c(n, 13) + 0.2 * sine_c(n, 97)
fs <- 128
cyc <- 16
k <- which.min(abs(r$freqs - cyc * fs/n))
y <- sine_c(n, 11) + 0.5 * sine_c(n, 71)
freqs <- (0:8)/8
m <- 12
cycles <- lapply(0:(m - 1), function(k) sine_c(n, 3) + 0.8 * 
    sin(2 * pi * 30 * (0:(n - 1))/n + k * 1.7))
Msc(x, y, nperseg = 128)
Cohere(x = x, y = y)
}
