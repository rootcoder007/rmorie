% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{clrgrf_cluster_jackknife}
\alias{clrgrf_cluster_jackknife}
\title{clrgrf_cluster_jackknife}
\usage{
clrgrf_cluster_jackknife(preds, bags, groups, correction = TRUE)
}
\arguments{
\item{preds}{A vector; its length is taken.}
\item{bags}{A vector; indexed elementwise.}
\item{groups}{A vector; its length is taken.}
\item{correction}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the clrgrf_native implementation. Called by \code{morie_clrgrf}. See the file header for the source the module follows.
}
\value{
A list with \code{variance}, \code{info}.
}
\keyword{internal}
