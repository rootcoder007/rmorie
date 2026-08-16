% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_http_post_json}
\alias{.morie_dataset_http_post_json}
\title{Synchronous POST + parse JSON. 3YY: routes through morie\'s C++}
\usage{
.morie_dataset_http_post_json(url, body, query = NULL, headers = character(), timeout_s = 60L, auto_unbox = TRUE)
}
\description{
libcurl .morie_http_post when available; falls back to httr2\'s req_body_json + req_perform + resp_body_json otherwise. Body (R list) is serialised via .s03json_toJSON.
}
\value{
The value of \code{httr2::resp_body_json}.
}
\keyword{internal}
