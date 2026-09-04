% Generated from the package source by gen_vic_rd.py -- roxygenise is not run on this branch
% Please edit documentation in R/datasets_vic.R
\name{morie_datasets_vic_table}
\alias{morie_datasets_vic_table}
\title{Read one Victorian crime data table}
\usage{
morie_datasets_vic_table(key, table = 1, cache_dir = NULL, offline = TRUE)
}
\arguments{
\item{key}{A catalog key; see \code{morie_datasets_vic_catalog()}.}

\item{table}{Sheet to read: an integer table number (1 gives
  "Table 01") or a sheet name.}

\item{cache_dir}{Optional cache directory.}

\item{offline}{When \code{TRUE} (the default) the function never
  downloads: it reads a cached copy and otherwise returns a 0-row
  frame, so examples and tests never touch the network.}

}
\value{
A \code{data.frame} of the requested table, or a 0-row frame
  when the workbook is not cached and \code{offline} is \code{TRUE}.
}
\description{
Loads a sheet from one of the Crime Statistics Agency workbooks,
downloading and caching the file on first use.
}
\references{
Crime Statistics Agency (Victoria), CC BY 4.0.
}
\examples{
# offline by default: returns a 0-row frame unless already cached
df <- morie_datasets_vic_table("criminal_incidents", table = 1)
ncol(df)
}
