% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_corrections_uof_load}
\alias{.morie_corrections_uof_load}
\title{Generic Corrections-UoF loader.}
\usage{
.morie_corrections_uof_load(key, offline = TRUE, resource_id = NULL, source = NULL)
}
\description{
Internal helper that every per-resource loader delegates to via the shared \code{.morie_load_chain()} (live -> included -> synthetic -> empty). Not exported.
}
\value{
The value of \code{.morie_load_chain}.
}
\keyword{internal}
