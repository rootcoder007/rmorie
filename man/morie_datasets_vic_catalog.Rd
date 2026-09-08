% Generated from the package source by gen_vic_rd.py -- roxygenise is not run on this branch
% Please edit documentation in R/datasets_vic.R
\name{morie_datasets_vic_catalog}
\alias{morie_datasets_vic_catalog}
\title{Catalog of the Victorian Crime Statistics Agency data tables}
\usage{
morie_datasets_vic_catalog()
}
\value{
A \code{data.frame} with one row per published workbook and
  the columns \code{key}, \code{file} and \code{url}.
}
\description{
Catalog of the Victorian Crime Statistics Agency data tables
}
\references{
Crime Statistics Agency (Victoria),
  \url{https://www.crimestatistics.vic.gov.au/}. Released under
  CC BY 4.0.
}
\examples{
cat_df <- morie_datasets_vic_catalog()
nrow(cat_df)
head(cat_df$key)
}
