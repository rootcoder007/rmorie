% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Smirnov2}
\alias{Smirnov2}
\title{Exact two-sided Smirnov distribution -- Sec. 6.3, p. 239}
\usage{
Smirnov2(d, m, n) 
}
\description{
Exact two-sided Smirnov distribution -- Sec. 6.3, p. 239
}
\arguments{
\item{d}{Argument \code{d}; see Usage.}
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
}
\value{
A list with \code{sf}, \code{cdf}, \code{npaths}, \code{inside}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Smirnov2(d = 0.4, m = 10, n = 12)
}
