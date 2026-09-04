% Generated from the package source by gen_vic_rd.py -- roxygenise is not run on this branch
% Please edit documentation in R/datasets_vic.R
\name{morie_datasets_vic_cache_dir}
\alias{morie_datasets_vic_cache_dir}
\title{Local cache directory for the Victorian workbooks}
\usage{
morie_datasets_vic_cache_dir(cache_dir = NULL)
}
\arguments{
\item{cache_dir}{Optional directory. Defaults to the
  \code{MORIE_VIC_CACHE} environment variable, then to a
  \code{vic} folder under \code{tools::R_user_dir("rmorie", "cache")}.}

}
\value{
The cache directory path, created if absent.
}
\description{
Local cache directory for the Victorian workbooks
}
\examples{
d <- morie_datasets_vic_cache_dir(tempfile())
dir.exists(d)
}
