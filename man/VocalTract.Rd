% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VocalTract}
\alias{VocalTract}
\title{VocalTract}
\usage{
VocalTract(y, fs, pitch_period = NULL, cutoff = NULL, pitch_range = c(0.002, 0.02))
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{pitch_period}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{cutoff}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{pitch_range}{A vector; indexed elementwise. Defaults to \code{c(0.002, 0.02)}.}
}
\description{
A step of the rangayyan_cep implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{response}, \code{cepstrum}, \code{cutoff}, \code{pitch_period}, \code{pitch_hz}, \code{peak_quefrency}, \code{fs}, \code{n}, \code{method}.
}
\keyword{internal}
