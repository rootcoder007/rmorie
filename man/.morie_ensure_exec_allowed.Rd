% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ensure_exec_allowed}
\alias{.morie_ensure_exec_allowed}
\title{Stop with a clear message if MORIE_NO_EXEC is set. Call at the head}
\usage{
.morie_ensure_exec_allowed(feature = "dynamic execution")
}
\arguments{
\item{feature}{Defaults to \code{"dynamic execution"}.}
}
\description{
of every sink that spawns an external process.
}
\value{
Invisibly,a logical value.
}
\keyword{internal}
\examples{
.morie_ensure_exec_allowed()
}
