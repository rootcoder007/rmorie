% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_acf}
\alias{.morie_rg_acf}
\title{.morie_rg_acf}
\usage{
.morie_rg_acf(x, lags)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{lags}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the rangayyan_adapt implementation. Called by \code{Acfseg}, \code{PcgSeg}, \code{PsdAcf} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_acf(V, V)
}
