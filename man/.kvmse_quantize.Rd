% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.kvmse_quantize}
\alias{.kvmse_quantize}
\title{.kvmse_quantize}
\usage{
.kvmse_quantize(y, cb)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{cb}{Passed to \code{outer}.}
}
\description{
A step of the tqkmse implementation. Called by \code{Kvmse}, \code{Kvquant}, \code{Vcquant}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.kvmse_quantize(V, V)
}
