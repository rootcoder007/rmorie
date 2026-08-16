% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lstm}
\alias{Lstm}
\title{Lstm}
\usage{
Lstm(sequences, labels = NULL, hidden = 8, ridge = 1e-06, seed = 1, weights = NULL)
}
\arguments{
\item{sequences}{See Usage.}
\item{labels}{Defaults to \code{NULL}.}
\item{hidden}{Defaults to \code{8}.}
\item{ridge}{Defaults to \code{1e-06}.}
\item{seed}{Defaults to \code{1}.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_class2 implementation; see the file header for the source it follows.
}
\value{
A list with \code{hidden}, \code{cell}, \code{predictions}, \code{accuracy}, \code{readout}, \code{classes}, \code{method}.
}
\keyword{internal}
