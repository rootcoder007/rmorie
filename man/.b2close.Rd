% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b2close}
\alias{.b2close}
\title{.b2close}
\usage{
.b2close(p)
}
\arguments{
\item{p}{Numeric; passed to \code{sum}.}
}
\description{
A step of the b2ot implementation. Called by \code{.b2sinkhorn}, \code{Otfreeen}, \code{Otsinktol}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.b2close(V)
}
