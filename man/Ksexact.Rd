% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ksexact}
\alias{Ksexact}
\title{Exact P(D_n < d) -- Theorem 4.3.2 via Durbin\'s matrix identity, p. 111}
\usage{
Ksexact(d, n) 
}
\description{
Exact P(D_n < d) -- Theorem 4.3.2 via Durbin\'s matrix identity, p. 111
}
\arguments{
\item{d}{Argument \code{d}; see Usage.}
\item{n}{Argument \code{n}; see Usage.}
}
\value{
A list with \code{cdf}, \code{sf}, \code{k}, \code{t}, \code{n}, \code{d}.
}
\keyword{internal}
\examples{
Ksexact(d = 5L, n = 5L)
}
