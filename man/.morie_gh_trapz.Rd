% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gh_trapz}
\alias{.morie_gh_trapz}
\title{.morie_gh_trapz}
\usage{
.morie_gh_trapz(xx, yy)
}
\arguments{
\item{xx}{See Usage.}
\item{yy}{See Usage.}
}
\description{
A step of the ghosal_native implementation. Called by \code{.morie_gh_hellinger}, \code{Ghosalfrsdensity}, \code{morie_polya_tree_density} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gh_trapz(V, V)
}
