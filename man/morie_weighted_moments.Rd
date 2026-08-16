% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_weighted_moments}
\alias{morie_weighted_moments}
\title{morie_weighted_moments}
\usage{
morie_weighted_moments(m, g)
}
\arguments{
\item{m}{A matrix; passed to \code{as.matrix}.}
\item{g}{See Usage.}
}
\description{
A step of the bndsmw_native implementation. Called by \code{morie_cvm_statistic}, \code{morie_gms_critical_value}. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{sd}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_weighted_moments(V, V)
}
