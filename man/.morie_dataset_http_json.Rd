% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_http_json}
\alias{.morie_dataset_http_json}
\title{GET + parse JSON. 3VV: when the C++ backend is available the}
\usage{
.morie_dataset_http_json(url, query = NULL, headers = character(), timeout_s = 60L)
}
\description{
body is fetched via libcurl then parsed via jsonlite (avoids the httr2 dep entirely); falls back to httr2 + httr2::resp_body_json otherwise.
}
\value{
The value of \code{httr2::resp_body_json}.
}
\keyword{internal}
