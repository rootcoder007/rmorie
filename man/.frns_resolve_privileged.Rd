% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frns_resolve_privileged}
\alias{.frns_resolve_privileged}
\title{Returns list(privileged = <key>, warning = <chr or NULL>)}
\usage{
.frns_resolve_privileged(privileged, rates)
}
\arguments{
\item{privileged}{Optional; may be \code{NULL}. Coerced to character by the body, with \code{as.character}.}
\item{rates}{Iterated over elementwise, with \code{vapply}.}
}
\description{
A step of the frns_metrics implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{privileged}, \code{warning}.
}
\keyword{internal}
