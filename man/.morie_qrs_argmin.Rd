% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_qrs_argmin}
\alias{.morie_qrs_argmin}
\title{.morie_qrs_argmin}
\usage{
.morie_qrs_argmin(v, lo, hi)
}
\arguments{
\item{v}{A vector; indexed elementwise.}
\item{lo}{Numeric; combined arithmetically in the body.}
\item{hi}{Passed to \code{:}.}
}
\description{
A step of the rangayyan_qrs implementation. Called by \code{DicNotch}, \code{EcgFeat}, \code{PpgFeat}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_qrs_argmin(v = c(1, 2, 3, 4, 5, 6, 7, 8), lo = c(1, 2, 3, 4, 5, 6, 7, 8), hi = c(1, 2, 3, 4, 5, 6, 7, 8))
}
