% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_trapz}
\alias{.fz_trapz}
\title{.fz_trapz}
\usage{
.fz_trapz(y, x)
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{x}{See Usage.}
}
\description{
A step of the fauzi_native implementation. Called by \code{morie_fauzi_b2_coefficient}, \code{morie_fauzi_boundary_free_kde}, \code{morie_fauzi_cumulative_survival_1} and 6 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.fz_trapz(V, V)
}
