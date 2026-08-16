% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mmd_gram}
\alias{.mmd_gram}
\title{Internal: Gram matrix for the three supported kernels}
\usage{
.mmd_gram(Z, kernel, gamma)
}
\arguments{
\item{Z}{A matrix; passed to \code{tcrossprod}.}
\item{kernel}{See Usage.}
\item{gamma}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the twosample implementation. Called by \code{morie_mmd_test}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.mmd_gram(Z = c(1, 2, 3, 4, 5, 6, 7, 8), kernel = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), gamma = 0.5)
}
