% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{narm_local_encoder}
\alias{narm_local_encoder}
\title{narm_local_encoder}
\usage{
narm_local_encoder(H, alpha)
}
\arguments{
\item{H}{See Usage.}
\item{alpha}{See Usage.}
}
\description{
Part of the narm_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
narm_local_encoder(V, V)
}
