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
\item{ic}{Passed to \code{naive_variance}.}
\item{cluster}{Passed to \code{cluster_variance}.}
}
\description{
A step of the tlclust_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{se_naive}, \code{se_clustered}, \code{ratio}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
design_effect(V, V)
}
