% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_stl_ma}
\alias{.mor_stl_ma}
\title{Moving average of length k; output length length(v) - k + 1}
\usage{
.mor_stl_ma(v, k)
}
\arguments{
\item{v}{A vector; its length is taken.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the stlAn_native implementation. Called by \code{morie_stl_decompose}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_stl_ma(V, V)
}
