% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{greedy_decode}
\alias{greedy_decode}
\title{greedy_decode}
\usage{
greedy_decode(emissions, labels) 
}
\arguments{
\item{emissions}{A matrix; passed to \code{as.matrix}.}
\item{labels}{A vector; indexed elementwise.}
}
\description{
A step of the benRea_native implementation. Called by \code{ner_decode}. See the file header for the source the module follows.
}
\value{
The value of \code{apply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
greedy_decode(V, V)
}
