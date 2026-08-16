% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_chain}
\alias{.morie_qrs_chain}
\title{Full Pan-Tompkins front end: bandpass, derivative, square, integrate}
\usage{
.morie_qrs_chain(x, fs)
}
\arguments{
\item{x}{Passed to \code{.morie_qrs_ptbp}.}
\item{fs}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_qrs implementation. Called by \code{QrsDetect}. See the file header for the source the module follows.
}
\value{
A list with \code{bp}, \code{dv}, \code{sq}, \code{ig}, \code{w}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_qrs_chain(V, V)
}
