% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DeltaArea}
\alias{DeltaArea}
\title{Eq (3.25): integral delta(t) dt = 1.  The property defines the delta,}
\usage{
DeltaArea(t = NULL, values = NULL, width = NULL)
}
\arguments{
\item{t}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_gridint}.}
\item{values}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_gridint}.}
\item{width}{Defaults to \code{NULL}.}
}
\description{
so the useful computation is the check on a candidate approximation.
}
\value{
A list with \code{area}, \code{unit_area}, \code{method}.
}
\keyword{internal}
\examples{
DeltaArea()
}
