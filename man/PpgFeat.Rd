% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PpgFeat}
\alias{PpgFeat}
\title{Section 1.2.11.  The notch is located with the SAME machinery the}
\usage{
PpgFeat(ppg, fs, mwin = 16) 
}
\arguments{
\item{ppg}{A vector; its length is taken and its elements indexed.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{mwin}{Passed to \code{DicNotch}. Defaults to \code{16}.}
}
\description{
book gives for the carotid pulse in Section 4.3.5 (eqs 4.22, 4.23), because the PPG pulse shape mirrors the carotid pulse.  The perfusion index says how much of the reading is pulsatile blood volume rather than static tissue absorption.
}
\value{
A list with \code{systolic}, \code{notch}, \code{diastolic}, \code{onset}, \code{amplitude}, \code{ac}, \code{dc}, \code{pi}, \code{rate}, \code{fs}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(8)
fs <- 100
tv <- seq(0, 5, by = 1 / fs)
ppg <- sin(2 * pi * 1.2 * tv) + 0.1 * rnorm(length(tv))
PpgFeat(ppg, fs = fs)
}
