% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_knn_index}
\alias{.morie_knn_index}
\title{.morie_knn_index}
\usage{
.morie_knn_index(coords, k)
}
\arguments{
\item{coords}{See Usage.}
\item{k}{See Usage.}
}
\description{
k-nearest-neighbour indices (Euclidean), the FNN::get.knn()$nn.index surface used here. Brute force O(n^2) -- the call sites feed spatial unit tables (hundreds of rows), where this is instant.
}
\value{
The value of \code{.morie_knn_index_cpp}.
}
\keyword{internal}
