% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_build_url}
\alias{.morie_dataset_build_url}
\title{Assemble a fully-formed URL with percent-encoded query parameters.}
\usage{
.morie_dataset_build_url(url, query = NULL)
}
\description{
Mirrors httr2::req_url_query() but doesn\'t require httr2 at the call site -- needed so the libcurl-backed C++ HTTP path can take a single, already-built URL.
}
\value{
A character value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_dataset_build_url(V)
}
