% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HSoundId}
\alias{HSoundId}
\title{Section 4.9.  S1 at the QRS onset; S2 from the dicrotic notch minus}
\usage{
HSoundId(ecg, cp, fs)
}
\arguments{
\item{ecg}{A vector; its length is taken.}
\item{cp}{A vector; its length is taken.}
\item{fs}{Numeric; combined arithmetically in the body.}
}
\description{
the standardised 52.6 ms (mean + 2 SD of the 42.6 +/- 5 ms Lehner and Rangayyan measurement).  The notch lags the sound because it is the same aortic-valve closure observed after propagation up the arterial tree.
}
\value{
A list with \code{s1}, \code{s2}, \code{notch}, \code{qrs}, \code{s2delayms}, \code{s2delaymeasured}, \code{searchwindowms}, \code{fs}, \code{method}.
}
\keyword{internal}
