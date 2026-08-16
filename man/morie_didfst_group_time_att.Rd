% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_didfst_group_time_att}
\alias{morie_didfst_group_time_att}
\title{Callaway-Sant\'Anna ATT(g,t) under staggered adoption}
\usage{
morie_didfst_group_time_att(Y, first_treated, comparison = "not-yet-treated")
}
\arguments{
\item{Y}{Numeric panel (n by T).}
\item{first_treated}{Numeric or list; the 1-based period at which   each unit first becomes treated, or \code{NULL} / \code{Inf} for   never-treated.}
\item{comparison}{One of \code{"never-treated"} or   \code{"not-yet-treated"}.}
}
\description{
Callaway-Sant\'Anna ATT(g,t) under staggered adoption
}
\value{
A list with \code{att}, \code{cohorts} and the cohort   details.
}
\keyword{internal}
