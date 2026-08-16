% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{format.morie_bigint}
\alias{format.morie_bigint}
\title{Format method for \code{morie_bigint} objects}
\usage{
format.morie_bigint(x, ...)
}
\arguments{
\item{x}{A \code{morie_bigint} object.}
\item{...}{Ignored; accepted for S3 consistency.}
}
\description{
Format method for \code{morie_bigint} objects
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
format.morie_bigint(V)
}
