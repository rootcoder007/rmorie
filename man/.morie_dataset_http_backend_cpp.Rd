% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_http_backend_cpp}
\alias{.morie_dataset_http_backend_cpp}
\title{Detect whether the C++ libcurl backend (morie_http.cpp, 3VV) is}
\usage{
.morie_dataset_http_backend_cpp()
}
\description{
available. The Rcpp export only exists when the package was built with libcurl visible to curl-config; on minimal CRAN macOS images the C++ symbols may be absent. Fallback: httr2.
}
\value{
The value of \code{exists}.
}
\keyword{internal}
\examples{
.morie_dataset_http_backend_cpp()
}
