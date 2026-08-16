% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_cov}
\alias{.morie_bx_cov}
\title{.morie_bx_cov}
\usage{
.morie_bx_cov(X, unbiased = TRUE)
}
\arguments{
\item{X}{A matrix; indexed by row and column.}
\item{unbiased}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the rangayyan_class2 implementation. Called by \code{.morie_bx_chsel}, \code{PvcBayes}. See the file header for the source the module follows.
}
\value{
A list with \code{mu}, \code{C}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_bx_cov(M)
}
