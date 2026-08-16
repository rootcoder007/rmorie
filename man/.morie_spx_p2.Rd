% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_spx_p2}
\alias{.morie_spx_p2}
\title{.morie_spx_p2}
\usage{
.morie_spx_p2(z)
}
\arguments{
\item{z}{Numeric; passed to \code{abs}.}
}
\description{
A step of the sp_fill implementation. Called by \code{LisaClust}, \code{MantelZ}, \code{MoranRes}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_spx_p2(V)
}
