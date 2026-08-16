% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_xcorr}
\alias{.morie_rg_xcorr}
\title{.morie_rg_xcorr}
\usage{
.morie_rg_xcorr(x, y, maxlag = NULL)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{y}{A vector; its length is taken and its elements indexed.}
\item{maxlag}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_corr implementation. Called by \code{MfAcf}, \code{RefPattern}. See the file header for the source the module follows.
}
\value{
A list with \code{lags}, \code{values}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_rg_xcorr(V, V)
}
