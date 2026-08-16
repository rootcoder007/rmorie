% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b2xlogx}
\alias{.b2xlogx}
\title{.b2xlogx}
\usage{
.b2xlogx(p, base = 2)
}
\arguments{
\item{p}{Numeric; combined arithmetically in the body.}
\item{base}{Passed to \code{.b2logb}. Defaults to \code{2}.}
}
\description{
A step of the b2info implementation. Called by \code{.b2ent}, \code{Difent}. See the file header for the source the module follows.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.b2xlogx(V)
}
