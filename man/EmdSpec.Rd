% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EmdSpec}
\alias{EmdSpec}
\title{EMD-based instantaneous-frequency spectrum, Sec 9.4 eqs (9.8)-(9.12)}
\usage{
EmdSpec(x, fs = 1, max_imfs = 8, nfreq = 32, tol = 0.05)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{Defaults to \code{1}.}
\item{max_imfs}{Defaults to \code{8}.}
\item{nfreq}{Defaults to \code{32}.}
\item{tol}{Defaults to \code{0.05}.}
}
\description{
Part of the rangayyan_tf implementation; see the file header for the source it follows.
}
\value{
A list with \code{spectrum}, \code{times}, \code{freqs}, \code{imfs}, \code{amplitude}, \code{inst_freq}, \code{marginal}, \code{n_imfs}, \code{peak_freq}, \code{method}.
}
\keyword{internal}
