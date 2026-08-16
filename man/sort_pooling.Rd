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
Part of the sortP_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{pooled}, \code{order}, \code{n_truncated}, \code{n_padded}, \code{k}, \code{sort_channel}, \code{note}.
}
\keyword{internal}
\examples{
sort_pooling(features = c(1, 2, 3, 4, 5, 6, 7, 8), k_keep = 5L)
}
