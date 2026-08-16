% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funcal_transfer_terms}
\alias{morie_funcal_transfer_terms}
\title{morie_funcal_transfer_terms}
\usage{
morie_funcal_transfer_terms(assignments, annotations, sources = NULL, min_support = 1)
}
\arguments{
\item{assignments}{See Usage.}
\item{annotations}{See Usage.}
\item{sources}{Defaults to \code{NULL}.}
\item{min_support}{Defaults to \code{1}.}
}
\description{
Part of the funcal_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_funcal_transfer_terms(V, V)
}
