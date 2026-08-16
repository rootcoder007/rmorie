% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{alfXent}
\alias{alfXent}
\title{Cross entropy -sum(y log p) for one distribution}
\usage{
alfXent(y, p, eps = 1e-12)
}
\arguments{
\item{y}{Numeric; combined arithmetically in the body.}
\item{p}{Passed to \code{pmax}.}
\item{eps}{Passed to \code{pmax}. Defaults to \code{1e-12}.}
}
\description{
A step of the alf_core implementation. Called by \code{Alfdgram}, \code{Alfplddt}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
alfXent(V, V)
}
