% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{wbwc_invert}
\alias{wbwc_invert}
\title{wbwc_invert}
\usage{
wbwc_invert(tree, known, candidates, priors, sensitive = 0L, unknown = NULL)
}
\arguments{
\item{tree}{See Usage.}
\item{known}{See Usage.}
\item{candidates}{See Usage.}
\item{priors}{See Usage.}
\item{sensitive}{Defaults to \code{0L}.}
\item{unknown}{Defaults to \code{NULL}.}
}
\description{
Part of the attrInf_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{scores}, \code{n_paths}, \code{N}.
}
\keyword{internal}
