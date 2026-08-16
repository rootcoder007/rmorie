% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HrFromCnt}
\alias{HrFromCnt}
\title{Eq (4.19): HR = 60 NB / T.  The counting estimate averages over the}
\usage{
HrFromCnt(nbeats, duration)
}
\arguments{
\item{nbeats}{See Usage.}
\item{duration}{See Usage.}
}
\description{
whole window, so unlike the beat-to-beat form of eq (4.20) it is insensitive to a single mis-detected interval.
}
\value{
A list with \code{hr}, \code{nbeats}, \code{duration}, \code{method}.
}
\keyword{internal}
