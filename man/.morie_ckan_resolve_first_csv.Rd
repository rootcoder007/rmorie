% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ckan_resolve_first_csv}
\alias{.morie_ckan_resolve_first_csv}
\title{Resolve a CKAN `package_name` to its first-CSV resource UUID}
\usage{
.morie_ckan_resolve_first_csv(package_name, ckan_base)
}
\arguments{
\item{package_name}{CKAN package slug.}
\item{ckan_base}{CKAN host base URL (e.g.,   `.MORIE_MONTREAL_CKAN_BASE` or `.MORIE_TORONTO_CKAN_BASE`).}
}
\description{
Phase 3FFF1. Internal helper used by [morie_datasets_load_by_key()] to look up an arbitrary CKAN package\'s primary CSV resource without requiring the caller to know the UUID. Hits `/action/package_show?id=<pkg>` then walks the `resources` list for the first row whose `format` (or `mimetype`) matches `"CSV"` (case-insensitive). Falls back to the bare first resource if no CSV is present and raises a clear error if the package has no resources at all.
}
\value{
Character resource UUID.
}
\keyword{internal}
