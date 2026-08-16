% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.resolve_acquisition}
\alias{.resolve_acquisition}
\title{Resolve an acquisition spelling to one of three rules}
\usage{
.resolve_acquisition(name)
}
\arguments{
\item{name}{Character; one of the spellings listed in   \code{.ACQUISITIONS}.}
}
\description{
Map a user-supplied name to one of the three rules of Snoek et al. (2012) Equations 1-3, mirroring Python \code{morie.fn.bayoptr.resolve_acquisition}. "ucb" and "lcb" name the same rule under two spellings; the paper writes the lower bound because it minimises.
}
\value{
Character; \code{"ei"}, \code{"pi"} or \code{"lcb"}.
}
\keyword{internal}
