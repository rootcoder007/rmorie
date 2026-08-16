% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_http_text_with_status}
\alias{.morie_dataset_http_text_with_status}
\title{Status-aware GET. 3ZZ: returns a list(body, status_code) so the}
\usage{
.morie_dataset_http_text_with_status(url, query = NULL, headers = character(), timeout_s = 60L)
}
\description{
caller can format custom error messages for 4xx (e.g. NamUs\'s 401/403 API-key handling). Routes through libcurl + httr2 fallback.
}
\value{
A list with \code{body}, \code{status_code}.
}
\keyword{internal}
