% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TWaveDet}
\alias{TWaveDet}
\title{Gritzali et al., Section 4.3.4.  The QRS is detected first, blanked}
\usage{
TWaveDet(chans, qrs, fs, tdur = 0.16)
}
\arguments{
\item{chans}{A vector; its length is taken.}
\item{qrs}{See Usage.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{tdur}{Numeric; combined arithmetically in the body. Defaults to \code{0.16}.}
}
\description{
to the isoelectric baseline, and the length transform of eq (4.21) re-run with the window at the average T duration.  Working ACROSS channels is what makes T detection tractable.
}
\value{
A list with \code{t}, \code{onset}, \code{offset}, \code{length}, \code{tdur}, \code{nchan}, \code{fs}, \code{method}.
}
\keyword{internal}
