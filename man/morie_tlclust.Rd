% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlclust}
\alias{morie_tlclust}
\title{The LTMLE-with-clustering entry point}
\usage{
morie_tlclust(Q_seq, H_seq, Y, cluster, ic = NULL)
}
\arguments{
\item{Q_seq}{See Usage.}
\item{H_seq}{See Usage.}
\item{Y}{See Usage.}
\item{cluster}{See Usage.}
\item{ic}{Defaults to \code{NULL}.}
}
\description{
A step of the tlclust_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list, whose contents depend on the branch taken; across the branches its names are \code{estimate}, \code{psi}, \code{se_clustered}, \code{se_naive}, \code{ci}, \code{n_clusters}, \code{design_effect}, \code{method}, \code{note}, \code{naive}, \code{pooled}, \code{sequential}.
}
\keyword{internal}
\examples{
morie_tlclust(Q_seq = c(1, 2, 3, 4, 5, 6, 7, 8), H_seq = c(1, 2, 3, 4, 5, 6, 7, 8), Y = c(1, 2, 3, 4, 5, 6, 7, 8), cluster = c(1, 2, 3, 4, 5, 6, 7, 8))
}
