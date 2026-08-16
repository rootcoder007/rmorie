% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_psd}
\alias{.morie_qrs_psd}
\title{One-sided periodogram; power in units^2/Hz}
\usage{
.morie_qrs_psd(x, fs)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{fs}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_qrs implementation. Called by \code{EcgEmgCpl}, \code{EdrSignal}, \code{HrvFreq} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{freqs}, \code{power}.
}
\keyword{internal}
