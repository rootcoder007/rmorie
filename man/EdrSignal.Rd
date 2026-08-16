% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EdrSignal}
\alias{EdrSignal}
\title{Section 2.2.4 for the physiology; the estimator is Arunachalam and}
\usage{
EdrSignal(x, qrs, fs, fsr = 4)
}
\arguments{
\item{x}{See Usage.}
\item{qrs}{See Usage.}
\item{fs}{See Usage.}
\item{fsr}{Defaults to \code{4}.}
}
\description{
Brown, Proc. IEEE EMBC 2009, pp. 5681-5684 (reference [52] of Chapter 2). Chest motion swings the cardiac electrical axis, so the R amplitude sampled once per beat is a respiratory signal without a respiration sensor.
}
\value{
A list with \code{edr}, \code{amp}, \code{times}, \code{resprate}, \code{fsr}, \code{nbeats}, \code{method}.
}
\keyword{internal}
