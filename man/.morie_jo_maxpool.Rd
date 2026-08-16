% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_jo_maxpool}
\alias{.morie_jo_maxpool}
\title{.morie_jo_maxpool}
\usage{
.morie_jo_maxpool(v, k)
}
\arguments{
\item{v}{A vector; its length is taken and its elements indexed.}
\item{k}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the ts_joseph implementation. Called by \code{morie_nhitsnet}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
.morie_jo_maxpool(v = c(1, 2, 3, 4, 5, 6, 7, 8), k = 5L)
}
