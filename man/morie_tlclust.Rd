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
Part of the tlclust_native implementation; see the file header for the source it follows.
}
\value{
A list, whose contents depend on the branch taken; across the branches its names are \code{estimate}, \code{psi}, \code{se_clustered}, \code{se_naive}, \code{ci}, \code{n_clusters}, \code{design_effect}, \code{method}, \code{note}, \code{naive}, \code{pooled}, \code{sequential}.
}
\keyword{internal}
