% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HrvAr}
\alias{HrvAr}
\title{The RR series is unevenly sampled by construction, so it is}
\usage{
HrvAr(rr, order = 16, fs = 4, nfreq = 512) 
}
\arguments{
\item{rr}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{16}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{4}.}
\item{nfreq}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{512}.}
}
\description{
resampled onto a uniform grid (4 Hz, comfortably above the 0.4 Hz top of the HF band) and modelled all-pole per Section 7.5.  Band edges follow the Task Force of the ESC and NASPE (1996): VLF 0.003-0.04, LF 0.04-0.15, HF 0.15-0.40 Hz. The mean is removed first: a mean RR of ~0.8 s dwarfs the variability and would leak a huge DC pole into the VLF band.
}
\value{
The value of \code{fit}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HrvAr(V)
}
