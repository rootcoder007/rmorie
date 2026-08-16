% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03lensH}
\alias{.s03lensH}
\title{Integral of gamma over the disc of radius t; 1 for t >= 2r}
\usage{
.s03lensH(t, r)
}
\arguments{
\item{t}{Numeric; combined arithmetically in the body.}
\item{r}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the matern implementation. Called by \code{Matern}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.s03lensH(t = 5L, r = 5L)
}
