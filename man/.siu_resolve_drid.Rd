% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.siu_resolve_drid}
\alias{.siu_resolve_drid}
\title{Resolve an SIU case number to its report drid.}
\usage{
.siu_resolve_drid(case_number)
}
\description{
Manifest first (fast, offline); when the case is newer than the bundled manifest (or fell into an over-probed placeholder drid), a polite live search of the directors-reports index resolves it, so self-serve callers work for ANY published case with no cache and no keys. Newest reports render first, so the live search usually hits within the first page.
}
\value{
The terminal expression of the body.
}
\keyword{internal}
