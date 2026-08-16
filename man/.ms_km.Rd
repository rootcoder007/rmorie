% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ms_km}
\alias{.ms_km}
\title{.ms_km}
\usage{
.ms_km(time, event)
}
\arguments{
\item{time}{Passed to \code{.ms_check}.}
\item{event}{Passed to \code{.ms_check}.}
}
\description{
A step of the survival_more implementation. Called by \code{Cif}, \code{Finegray}, \code{Landmark} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{time}, \code{surv}, \code{n_risk}, \code{n_event}, \code{greenwood}.
}
\keyword{internal}
\examples{
.ms_km(time = c(1, 2, 3, 4, 5, 6, 7, 8), event = c(0, 1, 0, 1, 1, 0, 1, 0))
}
