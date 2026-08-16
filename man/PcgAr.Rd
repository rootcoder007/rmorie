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
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{segment}{Optional; may be \code{NULL}. A vector; indexed elementwise.}
}
\description{
of S1 and S2.  A pole p gives a resonance at (fs/2pi) Arg(p) with bandwidth -(fs/pi) log|p|.  Only the upper half plane is reported: for a real signal the poles are conjugate pairs, and listing both halves would double-count every resonance.
}
\value{
The value of \code{fit}, as built in the body.
}
\keyword{internal}
