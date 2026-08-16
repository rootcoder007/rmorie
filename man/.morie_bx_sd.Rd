% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_sd}
\alias{.morie_bx_sd}
\title{.morie_bx_sd}
\usage{
.morie_bx_sd(v, ddof = 1)
}
\arguments{
\item{v}{A vector; its length is taken.}
\item{ddof}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{CadPipe}, \code{PvcBayes}, \code{VagClass}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_bx_sd(V)
}
