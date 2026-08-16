% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_ccf}
\alias{.morie_rg_ccf}
\title{Theta(k) = E[x(n-k) d(n)], the right-hand side of eq. (3.168)}
\usage{
.morie_rg_ccf(x, d, lags)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{d}{A vector; its length is taken and its elements indexed.}
\item{lags}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the rangayyan_adapt implementation. Called by \code{Whopf}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
.morie_rg_ccf(x = c(1, 2, 3, 4, 5, 6, 7, 8), d = c(1, 2, 3, 4, 5, 6, 7, 8), lags = 5L)
}
