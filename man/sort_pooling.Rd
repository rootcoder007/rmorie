% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sort_pooling}
\alias{sort_pooling}
\title{sort_pooling}
\usage{
sort_pooling(features, k_keep, sort_channel = -1)
}
\arguments{
\item{features}{See Usage.}
\item{k_keep}{See Usage.}
\item{sort_channel}{Defaults to \code{-1}.}
}
\description{
A step of the sortP_native implementation. Called by \code{order_is_graph_determined}. See the file header for the source the module follows.
}
\value{
A list with \code{pooled}, \code{order}, \code{n_truncated}, \code{n_padded}, \code{k}, \code{sort_channel}, \code{note}.
}
\keyword{internal}
\examples{
sort_pooling(features = c(1, 2, 3, 4, 5, 6, 7, 8), k_keep = 5L)
}
