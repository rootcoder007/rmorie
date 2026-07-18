% Generated manually to match R/ingest_cihi.R (roxygenise regenerates
% all man/ + reloads the compiled package, which is impractical on the
% dev host); keep in sync with the roxygen block on
% morie_datasets_cihi_data_tables().
\name{morie_datasets_cihi_data_tables}
\alias{morie_datasets_cihi_data_tables}
\title{Catalogue of CIHI open data-table workbooks}
\usage{
morie_datasets_cihi_data_tables()
}
\value{
A base R \code{data.frame} with columns \code{title}, \code{url}, and
(when \pkg{rmoriedata} is present) \code{wayback_url}.
}
\description{
Returns the CIHI data-table catalogue --- every public data-table
\code{.xlsx} workbook on CIHI's \dQuote{Access data and reports > Data
tables} page
(\url{https://www.cihi.ca/en/access-data-and-reports/data-tables}), with
its direct \code{url} and an Internet Archive \code{wayback_url}
fallback. The full ~218-row catalogue is bundled in \pkg{rmoriedata}
(\code{rmoriedata::load_cihi_data_tables()}); if that package is not
installed this returns a small built-in subset so the function still
works. Pass any \code{url} to \code{\link{morie_ingest_cihi_xlsx}}.
}
\examples{
cat <- morie_datasets_cihi_data_tables()
nrow(cat)
cat$title[1]
}
\seealso{
\code{\link{morie_ingest_cihi_xlsx}}
}
