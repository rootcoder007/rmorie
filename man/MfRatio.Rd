% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfRatio}
\alias{MfRatio}
\title{Eq (4.41).  Dividing by the constant E_x is what turns eq (4.39) into}
\usage{
MfRatio(X, H, freqs, t0, noise_power)
}
\arguments{
\item{X}{Coerced to complex by the body, with \code{as.complex}.}
\item{H}{Coerced to complex by the body, with \code{as.complex}.}
\item{freqs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{t0}{Passed to \code{MfPeak}.}
\item{noise_power}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
something Schwarz\'s inequality applies to; by eq (4.46) the ratio cannot exceed 2/P_eta_i, with equality exactly at eq (4.48).
}
\value{
A list with \code{ratio}, \code{bound}, \code{optimality}, \code{numerator}, \code{energy_h}, \code{energy_x}, \code{method}.
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
MfRatio(X = freqs, H = freqs, freqs = freqs, t0 = cyc, noise_power = cyc)
}
