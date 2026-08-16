% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{narm_softmax}
\alias{narm_softmax}
\title{narm_softmax}
\usage{
narm_softmax(z)
}
\arguments{
\item{z}{See Usage.}
}
\description{
A step of the narm_native implementation. Called by \code{narm_attention_weights}, \code{narm_bilinear_scores}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
narm_softmax(V)
}
