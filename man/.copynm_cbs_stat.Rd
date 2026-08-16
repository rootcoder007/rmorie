% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.copynm_cbs_stat}
\alias{.copynm_cbs_stat}
\title{CBS statistic Z_C = max_{i<j} |Z_{ij}| on v; returns list(z, i, j)}
\usage{
.copynm_cbs_stat(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
where indices are 0-based half-open into v (i < j), matching Python.
}
\value{
A list with \code{z}, \code{i}, \code{j}.
}
\keyword{internal}
