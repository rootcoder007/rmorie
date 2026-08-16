% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ragRet_ivf_search}
\alias{morie_ragRet_ivf_search}
\title{morie_ragRet_ivf_search}
\usage{
morie_ragRet_ivf_search(query, corpus, index, k.top = 5L, nprobe = 1L, metric = "inner_product")
}
\arguments{
\item{query}{See Usage.}
\item{corpus}{See Usage.}
\item{index}{See Usage.}
\item{k.top}{Defaults to \code{5L}.}
\item{nprobe}{Defaults to \code{1L}.}
\item{metric}{Defaults to \code{"inner_product"}.}
}
\description{
Part of the ragRet_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{indices}, \code{scores}, \code{comparisons}, \code{probed}, \code{n.cells}, \code{fraction.scanned}, \code{note}.
}
\keyword{internal}
