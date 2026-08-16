% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HClust}
\alias{HClust}
\title{Section 10.5.1.  Single linkage CHAINS -- it will string distant}
\usage{
HClust(X, linkage = "single", k = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{linkage}{Defaults to \code{"single"}.}
\item{k}{Defaults to \code{NULL}.}
}
\description{
clusters together through a bridge of intermediate points -- while complete linkage is compact and splits elongated clusters.  The choice is not cosmetic: on the same data they routinely give different partitions, which is why the full merge history is returned rather than only a labelling.
}
\value{
A list with \code{history}, \code{labels}, \code{linkage}, \code{n}, \code{k}, \code{merge_distances}, \code{monotonic_merges}, \code{single_linkage_chains}, \code{linkage_changes_the_partition}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
HClust(V)
}
