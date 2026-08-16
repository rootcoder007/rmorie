% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EcgFeat}
\alias{EcgFeat}
\title{Section 1.2.4 waves and intervals.  Amplitudes are measured against}
\usage{
EcgFeat(x, qrs, fs)
}
\arguments{
\item{x}{See Usage.}
\item{qrs}{See Usage.}
\item{fs}{See Usage.}
}
\description{
the PQ segment, not against zero: the PQ segment is the isoelectric reference the book names, and it absorbs any residual baseline offset.
}
\value{
A list with \code{pamp}, \code{qamp}, \code{ramp}, \code{samp}, \code{tamp}, \code{qrsdur}, \code{pdur}, \code{tdur}, \code{prdur}, \code{qtdur}, \code{qrsdurmean}, \code{prdurmean}, \code{qtdurmean}, \code{rampmean}, \code{nbeats}, \code{fs}, \code{method}.
}
\keyword{internal}
