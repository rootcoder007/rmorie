% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EcgEmgCpl}
\alias{EcgEmgCpl}
\title{Section 2.2.6 (EMG RMS and mean frequency rise with contraction)}
\usage{
EcgEmgCpl(ecg, emg, qrs, fs)
}
\arguments{
\item{ecg}{A vector; its length is taken.}
\item{emg}{A vector; its length is taken and its elements indexed.}
\item{qrs}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Numeric; combined arithmetically in the body.}
}
\description{
meets Section 2.2.5 (rate rises with effort).  Both series are sampled on the same beat grid so the correlation between them is well defined.
}
\value{
A list with \code{hr}, \code{rms}, \code{meanfreq}, \code{rrms}, \code{rmnf}, \code{nbeats}, \code{fs}, \code{method}.
}
\keyword{internal}
