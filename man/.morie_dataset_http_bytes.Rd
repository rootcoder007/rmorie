% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dataset_http_bytes}
\alias{.morie_dataset_http_bytes}
\title{Binary-safe GET. 3XX: routes through morie\'s C++ libcurl}
\usage{
.morie_dataset_http_bytes(url, query = NULL, headers = character(), timeout_s = 60L)
}
\description{
.morie_http_get_bytes when available; falls back to httr2\'s resp_body_raw otherwise. Returns a raw vector (no NUL truncation). Used for shapefile zips, FGDB zips, PDFs, KMZ, and any other binary payload the text-returning helpers would corrupt.
}
\value{
The value of \code{httr2::resp_body_raw}.
}
\keyword{internal}
