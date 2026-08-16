% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{design_effect}
\alias{design_effect}
\title{design_effect}
\usage{
design_effect(ic, cluster)
}
\arguments{
\item{ic}{See Usage.}
\item{cluster}{See Usage.}
}
\description{
Part of the tlclust_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{se_naive}, \code{se_clustered}, \code{ratio}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
design_effect(V, V)
}
