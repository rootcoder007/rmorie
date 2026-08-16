% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sentpc_units}
\alias{.sentpc_units}
\title{Split an escaped string on the marker}
\usage{
.sentpc_units(escaped)
}
\arguments{
\item{escaped}{Character.}
}
\description{
Every unit after the first begins with U+2581 so a naive \code{strsplit} on the marker (and re-attaching one) would collapse runs of spaces, breaking the lossless identity.
}
\value{
Character vector of units.
}
\keyword{internal}
