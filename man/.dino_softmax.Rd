% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dino_softmax}
\alias{.dino_softmax}
\title{.dino_softmax}
\usage{
.dino_softmax(v, tau, center = NULL)
}
\arguments{
\item{v}{A vector; its length is taken.}
\item{tau}{Numeric; combined arithmetically in the body.}
\item{center}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the dinmlt implementation. Called by \code{Dinmlt}, \code{Dinov2}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.dino_softmax(V, V)
}
