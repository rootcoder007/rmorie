% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ks2asymp}
\alias{Ks2asymp}
\title{Two-sample KS asymptotic -- Sec. 6.3, p. 241}
\usage{
Ks2asymp(d, m, n) 
}
\description{
Two-sample KS asymptotic -- Sec. 6.3, p. 241
}
\arguments{
\item{d}{Argument \code{d}; see Usage.}
\item{m}{Argument \code{m}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
}
\value{
A list with \code{p_value}, \code{k}, \code{neff}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
Ks2asymp(d = 5L, m = 5L, n = 5L)
}
