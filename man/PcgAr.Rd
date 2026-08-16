% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcgAr}
\alias{PcgAr}
\title{Chapter 7: the poles of an all-pole PCG model track the resonances}
\usage{
PcgAr(x, fs, order = NULL, segment = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{order}{Defaults to \code{NULL}.}
\item{segment}{Defaults to \code{NULL}.}
}
\description{
of S1 and S2.  A pole p gives a resonance at (fs/2pi) Arg(p) with bandwidth -(fs/pi) log|p|.  Only the upper half plane is reported: for a real signal the poles are conjugate pairs, and listing both halves would double-count every resonance.
}
\value{
The value of \code{fit}, as built in the body.
}
\keyword{internal}
