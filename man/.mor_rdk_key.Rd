% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_rdk_key}
\alias{.mor_rdk_key}
\title{Canonical fixed-width key of a sorted bond-index tuple: usable for}
\usage{
.mor_rdk_key(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
ordering because every tuple compared shares a length
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_rdk_key(V)
}
