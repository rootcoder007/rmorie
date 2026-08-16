% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ragRet_top_k}
\alias{morie_ragRet_top_k}
\title{morie_ragRet_top_k}
\usage{
morie_ragRet_top_k(query, corpus, k.top = 5L, metric = "inner_product")
}
\arguments{
\item{query}{See Usage.}
\item{corpus}{See Usage.}
\item{k.top}{Defaults to \code{5L}.}
\item{metric}{Compared against \code{"cosine"}. Defaults to \code{"inner_product"}.}
}
\description{
A step of the ragRet_native implementation. Called by \code{morie_ragRet_ivf_search}. See the file header for the source the module follows.
}
\value{
A list with \code{indices}, \code{scores}, \code{all_scores}, \code{metric}, \code{comparisons}, \code{note}.
}
\keyword{internal}
\examples{
morie_ragRet_top_k(query = 5L, corpus = c(1, 2, 3, 4, 5, 6, 7, 8))
}
