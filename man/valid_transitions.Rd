% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{valid_transitions}
\alias{valid_transitions}
\title{valid_transitions}
\usage{
valid_transitions(labels)
}
\arguments{
\item{labels}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the benRea_native implementation. Called by \code{viterbi_decode}. See the file header for the source the module follows.
}
\value{
The value of \code{T}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
valid_transitions(V)
}
