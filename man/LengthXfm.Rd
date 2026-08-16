% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LengthXfm}
\alias{LengthXfm}
\title{Eq (4.21) (Gritzali et al.).  Since (dx/dt) dt = dx, the transform is}
\usage{
LengthXfm(chans, wwin, fs)
}
\arguments{
\item{chans}{See Usage.}
\item{wwin}{See Usage.}
\item{fs}{See Usage.}
}
\description{
the arc length of the MULTICHANNEL trajectory accumulated over a w-second window.  Summing the squared derivative across channels before integrating is the whole point: a wave well defined in only one lead still contributes.
}
\value{
A list with \code{length}, \code{nchan}, \code{wsamp}, \code{wsec}, \code{fs}, \code{n}, \code{method}.
}
\keyword{internal}
