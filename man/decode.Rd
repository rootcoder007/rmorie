% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{decode}
\alias{decode}
\title{decode}
\usage{
decode(z, Wp, bp, items = NULL, activation = "sigmoid")
}
\arguments{
\item{z}{See Usage.}
\item{Wp}{See Usage.}
\item{bp}{See Usage.}
\item{items}{Defaults to \code{NULL}.}
\item{activation}{Defaults to \code{"sigmoid"}.}
}
\description{
Part of the cdaeRC_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{sapply}.
}
\keyword{internal}
\examples{
decode(z = c(1, 2, 3, 4, 5, 6, 7, 8), Wp = c(1, 2, 3, 4, 5, 6, 7, 8), bp = c(1, 2, 3, 4, 5, 6, 7, 8))
}
