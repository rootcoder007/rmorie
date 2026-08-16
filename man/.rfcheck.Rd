% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfcheck}
\alias{.rfcheck}
\title{.rfcheck}
\usage{
.rfcheck(X, Y)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{Y}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{Rfmdi}, \code{Rfmlt}, \code{Rfpmi}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.rfcheck(M, M)
}
