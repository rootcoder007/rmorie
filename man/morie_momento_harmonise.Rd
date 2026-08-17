% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_momento_harmonise}
\alias{morie_momento_harmonise}
\title{Harmonise a list of series into one batch}
\usage{
morie_momento_harmonise(series_list, patch_len, normalise = TRUE)
}
\arguments{
\item{series_list}{A list of series (each a list of numeric rows,   i.e. a time x channel matrix in row form).}
\item{patch_len}{Patch length.}
\item{normalise}{Apply per-series mean/sd normalisation.}
}
\description{
Per-series normalisation, truncation to a whole number of patches, channels kept independent.
}
\value{
A list with batch, meta, n_series, n_patches, patch_len,   note.
}
\keyword{internal}
\examples{
set.seed(1)
series_list <- list(rnorm(20), rnorm(24), rnorm(18))
morie_momento_harmonise(series_list, patch_len = 4)
}
