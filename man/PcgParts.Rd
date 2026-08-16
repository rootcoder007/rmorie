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
\item{pcg}{A vector; its length is taken and its elements indexed.}
\item{ecg}{A vector; its length is taken.}
\item{cp}{A vector; its length is taken.}
\item{fs}{Passed to \code{.morie_qrs_fs}.}
}
\description{
ECG and the carotid pulse because S1 and S2 are not reliably the loudest events in a PCG once murmurs are present.
}
\value{
A list with \code{s1}, \code{s2}, \code{systole}, \code{diastole}, \code{systolerms}, \code{diastolerms}, \code{fs}, \code{method}.
}
\keyword{internal}
