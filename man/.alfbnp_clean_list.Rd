% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfbnp_clean_list}
\alias{.alfbnp_clean_list}
\title{May arrive as a list of n x 3 matrices, or as a single 3-D array}
\usage{
.alfbnp_clean_list(clean)
}
\arguments{
\item{clean}{A matrix; indexed by row and column.}
}
\description{
(k x n x 3) -- which is what jsonlite hands back for a nested list, and what a caller who stacked their structures would naturally pass. lapply over a 3-D array walks the wrong dimension and silently produces garbage, so normalise before anything touches it.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
