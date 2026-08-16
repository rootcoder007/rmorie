% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CrossSpec}
\alias{CrossSpec}
\title{Raw cross-periodogram S_xy(w) = X(w) conj(Y(w)) / (2 pi n) on}
\usage{
CrossSpec(x, y)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
}
\description{
MEAN-REMOVED records (Brillinger 2001, Ch. 7).  NOT in Schabenberger & Gotway -- a fixed-string search for "cross-spectr" finds one bibliography entry and no method.  Zero frequency dropped.
}
\value{
A list with \code{omega}, \code{cospectrum}, \code{quadrature}, \code{amplitude}, \code{phase}, \code{means_removed}, \code{raw_not_consistent}, \code{n}, \code{method}.
}
\keyword{internal}
