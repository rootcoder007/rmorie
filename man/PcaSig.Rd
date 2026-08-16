% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PcaSig}
\alias{PcaSig}
\title{Section 9.7.1, eqs (9.37)-(9.41).  Multichannel recordings pick up}
\usage{
PcaSig(X, ncomp = NULL)
}
\arguments{
\item{X}{Passed to \code{.morie_bx_mat}.}
\item{ncomp}{Defaults to \code{NULL}.}
}
\description{
the same sources through different paths, so the channels are redundant; rotating onto the covariance eigenvectors puts most power in a few components and makes the truncation error the sum of the DISCARDED eigenvalues, which is why decreasing order minimises the MSE.
}
\value{
A list with \code{components}, \code{eigenvalues}, \code{eigenvectors}, \code{mean}, \code{covariance}, \code{varexplained}, \code{mse}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
PcaSig(M)
}
