% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Msc}
\alias{Msc}
\title{The square of eq (4.32).  The two forms are NOT interchangeable:}
\usage{
Msc(x, y, fs = 1, nperseg = NULL, noverlap = NULL)
}
\arguments{
\item{x}{Passed to \code{Cohere}.}
\item{y}{Passed to \code{Cohere}.}
\item{fs}{Passed to \code{Cohere}. Defaults to \code{1}.}
\item{nperseg}{Passed to \code{Cohere}.}
\item{noverlap}{Passed to \code{Cohere}.}
}
\description{
0.5 magnitude coherence is 0.25 magnitude-squared coherence.
}
\value{
The value of \code{r}, as built in the body.
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
}
