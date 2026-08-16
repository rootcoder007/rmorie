% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DicNotch}
\alias{DicNotch}
\title{Lehner and Rangayyan, Section 4.3.5.  The noncausal least-squares}
\usage{
DicNotch(cp, fs, qrs = NULL, mwin = 16)
}
\arguments{
\item{cp}{A vector; its length is taken and its elements indexed.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{qrs}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{mwin}{Numeric; combined arithmetically in the body. Defaults to \code{16}.}
}
\description{
second derivative of eq (4.22), squared and smoothed by eq (4.23).  The second derivative is used because the notch rides on the falling limb of the pulse, which a first derivative cannot separate from the limb itself.
}
\value{
A list with \code{notch}, \code{upstroke}, \code{s}, \code{p}, \code{mwin}, \code{fs}, \code{tolerancems}, \code{method}.
}
\keyword{internal}
