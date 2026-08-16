% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ExerEcgSt}
\alias{ExerEcgSt}
\title{ST level at J + jofs relative to the PQ isoelectric level, and the}
\usage{
ExerEcgSt(x, qrs, fs, jofs = 0.06, thresh = 0.1)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{qrs}{See Usage.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{jofs}{Numeric; combined arithmetically in the body. Defaults to \code{0.06}.}
\item{thresh}{Defaults to \code{0.1}.}
}
\description{
slope of the segment that follows (Section 1.2.4).  The 0.1 mV threshold is the conventional clinical figure, NOT a book value -- the payload says so.
}
\value{
A list with \code{stdev}, \code{stslope}, \code{pattern}, \code{stdevmean}, \code{stslopemean}, \code{flagged}, \code{thresh}, \code{threshnote}, \code{jofs}, \code{fs}, \code{method}.
}
\keyword{internal}
