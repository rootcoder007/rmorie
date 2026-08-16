% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcgParts}
\alias{PcgParts}
\title{The six-step procedure of Section 4.9.  Timing is imported from the}
\usage{
PcgParts(pcg, ecg, cp, fs)
}
\arguments{
\item{pcg}{See Usage.}
\item{ecg}{See Usage.}
\item{cp}{See Usage.}
\item{fs}{See Usage.}
}
\description{
ECG and the carotid pulse because S1 and S2 are not reliably the loudest events in a PCG once murmurs are present.
}
\value{
A list with \code{s1}, \code{s2}, \code{systole}, \code{diastole}, \code{systolerms}, \code{diastolerms}, \code{fs}, \code{method}.
}
\keyword{internal}
