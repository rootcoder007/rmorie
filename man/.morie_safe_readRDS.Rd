% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_safe_readRDS}
\alias{.morie_safe_readRDS}
\title{Single choke point for readRDS. readRDS can execute code while}
\usage{
.morie_safe_readRDS(path, feature = "reading an .rds file")
}
\arguments{
\item{path}{Passed to \code{readRDS}.}
\item{feature}{Passed to \code{sprintf}. Defaults to \code{"reading an .rds file"}.}
}
\description{
loading (S4/ALTREP/reference-object unserialize hooks), so a locked-down environment can forbid it entirely via MORIE_NO_EXEC. By default it loads normally -- the path is user-chosen or a first-party cache file; provenance is the caller\\'s responsibility. Feed it only .rds files you trust.
}
\value{
The value of \code{readRDS}.
}
\keyword{internal}
