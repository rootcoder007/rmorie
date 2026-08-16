% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_dwtstep}
\alias{.tf_dwtstep}
\title{.tf_dwtstep}
\usage{
.tf_dwtstep(a, h, g)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{h}{A vector; its length is taken.}
\item{g}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_dwt}, \code{Wpt}. See the file header for the source the module follows.
}
\value{
A list with \code{lo}, \code{hi}.
}
\keyword{internal}
\examples{
.tf_dwtstep(a = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5, g = c(1, 2, 3, 4, 5, 6, 7, 8))
}
