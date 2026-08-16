% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VfDetect}
\alias{VfDetect}
\title{Sections 1.2.4 and 8.11 describe VF but give NO detector and NO}
\usage{
VfDetect(x, fs, win = 4, conc = 0.6, crest = 4)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{win}{Defaults to \code{4}.}
\item{conc}{Defaults to \code{0.6}.}
\item{crest}{Defaults to \code{4}.}
}
\description{
threshold, and no external primary source was verified here.  The rule below is therefore stated as what it is: a two-part heuristic from the two properties the book DOES assert -- no discrete QRS complexes (so the integrator crest factor is small) and quasi-sinusoidal 4-7 Hz activity (so the spectral concentration is high).
}
\value{
A list with \code{flag}, \code{domfreq}, \code{concentration}, \code{crest}, \code{rate}, \code{conc}, \code{crestmax}, \code{nwin}, \code{fraction}, \code{win}, \code{fs}, \code{method}.
}
\keyword{internal}
