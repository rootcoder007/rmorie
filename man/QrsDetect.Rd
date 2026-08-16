% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{QrsDetect}
\alias{QrsDetect}
\title{Pan-Tompkins, Section 4.3.2, eqs (4.8)-(4.18) plus search-back.  The}
\usage{
QrsDetect(x, fs = 200)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Defaults to \code{200}.}
}
\description{
filter coefficients are integers designed for fs = 200 Hz and DO NOT rescale; the timing constants (150 ms integrator, 200 ms refractory) do.
}
\value{
A list with \code{qrs}, \code{rr}, \code{hr}, \code{integrated}, \code{bandpass}, \code{delay}, \code{spki}, \code{npki}, \code{thresh1}, \code{thresh2}, \code{searchback}, \code{fs}, \code{fsnote}, \code{method}.
}
\keyword{internal}
