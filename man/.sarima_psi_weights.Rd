% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_psi_weights}
\alias{.sarima_psi_weights}
\title{.sarima_psi_weights}
\usage{
.sarima_psi_weights(ar, ma, h)
}
\arguments{
\item{ar}{A vector; its length is taken and its elements indexed.}
\item{ma}{A vector; its length is taken and its elements indexed.}
\item{h}{Passed to \code{:}.}
}
\description{
A step of the sarima_native implementation. Called by \code{forecast}. See the file header for the source the module follows.
}
\value{
The value of \code{psi}, as built in the body.
}
\keyword{internal}
\examples{
.sarima_psi_weights(ar = c(1, 2, 3, 4, 5, 6, 7, 8), ma = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5)
}
