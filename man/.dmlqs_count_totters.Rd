% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dmlqs_count_totters}
\alias{.dmlqs_count_totters}
\title{.dmlqs_count_totters}
\usage{
.dmlqs_count_totters(adj, length = 3L, exclude_reverse = TRUE)
}
\arguments{
\item{adj}{Passed to \code{.dmlqs_norm_adj}.}
\item{length}{Defaults to \code{3L}.}
\item{exclude_reverse}{Defaults to \code{TRUE}.}
}
\description{
A step of the dmlqs_native implementation. Called by \code{morie_dmlqs}. See the file header for the source the module follows.
}
\value{
A list with \code{paths}, \code{totters}, \code{fraction}, \code{excluded_reverse}.
}
\keyword{internal}
