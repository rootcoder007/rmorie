% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b2pnorm}
\alias{.b2pnorm}
\title{.b2pnorm}
\usage{
.b2pnorm(p)
}
\arguments{
\item{p}{Numeric; passed to \code{sum}.}
}
\description{
A step of the b2info implementation. Called by \code{Compshan}, \code{Crsent}, \code{Predcomp} and 3 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.b2pnorm(V)
}
