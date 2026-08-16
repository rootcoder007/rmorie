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
\item{assignments}{A vector; indexed elementwise.}
\item{annotations}{A vector; indexed elementwise.}
\item{sources}{Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.}
\item{min_support}{Defaults to \code{1}.}
}
\description{
A step of the funcal_native implementation. Called by \code{morie_funcal}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_funcal_transfer_terms(V, V)
}
