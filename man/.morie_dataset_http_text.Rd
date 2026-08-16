% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_http_text}
\alias{.morie_dataset_http_text}
\title{GET that returns the response body as a UTF-8 character string.}
\usage{
.morie_dataset_http_text(url, query = NULL, headers = character(), timeout_s = 60L)
}
\description{
3VV: routes through morie\'s C++ libcurl backend (.morie_http_get, src/morie_http.cpp) when available; falls back to httr2 if not.
}
\value{
The value of \code{httr2::resp_body_string}.
}
\keyword{internal}
