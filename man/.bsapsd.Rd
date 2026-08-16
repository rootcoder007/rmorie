% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsapsd}
\alias{.bsapsd}
\title{Periodogram of x sampled at fs Hz with a Hann window, 0 .. fs/2}
\usage{
.bsapsd(x, fs, detrend = TRUE)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{detrend}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{ApneaDet}, \code{CadSpec}, \code{EggFeat} and 7 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{freqs}, \code{power}.
}
\keyword{internal}
