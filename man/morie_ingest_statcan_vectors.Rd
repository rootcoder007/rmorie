% Generated manually to match R/ingest_statcan.R (roxygenise regenerates
% all man/ + reloads the compiled package, which is impractical on the
% dev host); keep in sync with the roxygen block on
% morie_ingest_statcan_vectors().
\name{morie_ingest_statcan_vectors}
\alias{morie_ingest_statcan_vectors}
\title{Fetch StatCan series by vector ID (keyless WDS, latest-N periods)}
\usage{
morie_ingest_statcan_vectors(vectors, periods = 12L, timeout = 60L)
}
\arguments{
\item{vectors}{Character or numeric StatCan vector IDs. A leading
\code{"v"} is accepted and stripped (e.g. \code{"v41690973"} or
\code{41690973}).}

\item{periods}{Number of most-recent reference periods to return per
vector.}

\item{timeout}{Per-request timeout, seconds.}
}
\value{
A base R \code{data.frame}, one row per (vector, period):
\code{vector}, \code{ref_date}, \code{value}, \code{decimals},
\code{scalar_factor}, \code{symbol_code}, \code{release_time}.
}
\description{
Retrieves specific data series by their StatCan vector identifiers
through the public Web Data Service
\code{getDataFromVectorsAndLatestNPeriods} endpoint. This is the
small, targeted alternative to \code{\link{morie_ingest_statcan_cansim}},
which downloads a whole table (millions of rows). No credentials are
required; the WDS is public. The POST runs through rmorie's native
libcurl backend --- no extra package dependency.
}
\examples{
\dontshow{if (requireNamespace("jsonlite", quietly = TRUE) && requireNamespace("httr2", quietly = TRUE)) withAutoprint(\{ # examplesIf}
\donttest{
# Two CPI series, last 3 periods each -- no API key needed.
morie_ingest_statcan_vectors(c("v41690973", "v41691045"), periods = 3)
\dontshow{\}) # examplesIf}
}
}
\seealso{
\code{\link{morie_ingest_statcan_cansim}}
}
