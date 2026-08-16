% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ApneaEdr}
\alias{ApneaEdr}
\title{Section 10.2.5 frames the problem and the AHI but gives NO detection}
\usage{
ApneaEdr(edr, spo2, fs, hours = NULL, mindur = 10, desat = 3)
}
\arguments{
\item{edr}{A vector; its length is taken.}
\item{spo2}{A vector; its length is taken and its elements indexed.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{hours}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{mindur}{Numeric; combined arithmetically in the body. Defaults to \code{10}.}
\item{desat}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3}.}
}
\description{
algorithm, so none is attributed to it.  Requiring BOTH a respiratory pause and an SpO2 desaturation is what keeps a motion-induced flat stretch of EDR from being scored as an apnea.
}
\value{
A list with \code{events}, \code{nevents}, \code{ahi}, \code{desatdepth}, \code{hours}, \code{mindur}, \code{desat}, \code{fs}, \code{method}.
}
\keyword{internal}
