% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{automatic_bandwidth}
\alias{automatic_bandwidth}
\title{automatic_bandwidth}
\usage{
automatic_bandwidth(v, kernel = "qs", weights = NULL, n = NULL)
}
\arguments{
\item{v}{See Usage.}
\item{kernel}{Defaults to \code{"qs"}.}
\item{weights}{Defaults to \code{NULL}.}
\item{n}{Defaults to \code{NULL}.}
}
\description{
Part of the andmnh_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{bandwidth}, \code{alpha}, \code{fits}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
automatic_bandwidth(V)
}
