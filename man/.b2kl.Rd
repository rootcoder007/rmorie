% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b2kl}
\alias{.b2kl}
\title{.b2kl}
\usage{
.b2kl(p, q, base = 2)
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
\item{q}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{base}{Passed to \code{.b2logb}. Defaults to \code{2}.}
}
\description{
A step of the b2info implementation. Called by \code{Klchain}, \code{Predcomp}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.b2kl(V, V)
}
