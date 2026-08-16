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
\item{corpus}{A vector; indexed elementwise.}
\item{index}{A list; the body reads \code{$centroids}, \code{$lists}, \code{$n}, \code{$n.cells} from it.}
\item{k.top}{Defaults to \code{5L}.}
\item{nprobe}{Defaults to \code{1L}.}
\item{metric}{Passed to \code{morie_ragRet_top_k}. Defaults to \code{"inner_product"}.}
}
\description{
A step of the ragRet_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{indices}, \code{scores}, \code{comparisons}, \code{probed}, \code{n.cells}, \code{fraction.scanned}, \code{note}.
}
\keyword{internal}
