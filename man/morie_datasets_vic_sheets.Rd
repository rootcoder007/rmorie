% Generated from the package source by gen_vic_rd.py -- roxygenise is not run on this branch
% Please edit documentation in R/datasets_vic.R
\name{morie_datasets_vic_sheets}
\alias{morie_datasets_vic_sheets}
\title{Sheet names in a cached Victorian workbook}
\usage{
morie_datasets_vic_sheets(key, cache_dir = NULL)
}
\arguments{
\item{key}{A catalog key; see \code{morie_datasets_vic_catalog()}.}

\item{cache_dir}{Optional cache directory.}
}
\value{
A character vector of sheet names, or an empty vector when
  the workbook is not cached.
}
\description{
Sheet names in a cached Victorian workbook
}
\examples{
morie_datasets_vic_sheets("criminal_incidents")
}
