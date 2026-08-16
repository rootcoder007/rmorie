% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfPsd}
\alias{MfPsd}
\title{Eq (4.57): Y(f) = X X* = S_x(f).  Because Y is a PSD it is real and}
\usage{
MfPsd(x, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{dt}{Defaults to \code{1}.}
}
\description{
nonnegative -- the phase has been cancelled exactly, the frequency-domain statement of the coherent addition at t0.
}
\value{
A list with \code{Y}, \code{psd}, \code{max_imaginary}, \code{max_difference}, \code{is_psd}, \code{n}, \code{method}.
}
\keyword{internal}
