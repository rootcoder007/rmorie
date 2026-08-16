% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_didfst_aggregate_att}
\alias{morie_didfst_aggregate_att}
\title{Aggregate ATT(g,t)}
\usage{
morie_didfst_aggregate_att(gt, scheme = "simple", horizon = NULL)
}
\arguments{
\item{gt}{A \code{group_time_att} result.}
\item{scheme}{One of \code{"simple"}, \code{"event"} or   \code{"cohort"}.}
\item{horizon}{Optional integer, restrict the event-time profile.}
}
\description{
Aggregate ATT(g,t)
}
\value{
A list with the estimate and the scheme details.
}
\keyword{internal}
