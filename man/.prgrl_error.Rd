% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.prgrl_error}
\alias{.prgrl_error}
\title{.prgrl_error}
\usage{
.prgrl_error(X, y, w)
}
\arguments{
\item{X}{A vector; its length is taken and its elements indexed.}
\item{y}{A vector; indexed elementwise.}
\item{w}{A vector; indexed elementwise.}
}
\description{
A step of the prgrl_native implementation. Called by \code{easy_only_fit}, \code{prgrl}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.prgrl_error(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8))
}
