% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_did_modern_frame}
\alias{.morie_did_modern_frame}
\title{Shared design helper: long panel -> cohort (first treated period,}
\usage{
.morie_did_modern_frame(data, outcome, unit, time, treatment_time)
}
\arguments{
\item{data}{A vector; indexed elementwise.}
\item{outcome}{Passed to \code{c}.}
\item{unit}{Passed to \code{c}.}
\item{time}{Passed to \code{c}.}
\item{treatment_time}{Passed to \code{c}.}
}
\description{
NA = never), event time, and the treated-post indicator.
}
\value{
A list with \code{y}, \code{unit}, \code{time}, \code{g}, \code{rel}, \code{treated_post}.
}
\keyword{internal}
