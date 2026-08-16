% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsaacf}
\alias{.bsaacf}
\title{Biased autocorrelation of the mean-removed x, lags 0..maxlag}
\usage{
.bsaacf(x, maxlag)
}
\arguments{
\item{x}{See Usage.}
\item{maxlag}{Numeric; passed to \code{min}.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{.bsalpc}, \code{InfantCry}, \code{PdMonitor} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
