% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ml_gram}
\alias{.morie_ml_gram}
\title{.morie_ml_gram}
\usage{
.morie_ml_gram(a, sigma = NULL)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{sigma}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
}
\description{
A step of the causal_molak implementation. Called by \code{morie_hsicstat}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_ml_gram(V)
}
