% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_rrf_score}
\alias{morie_kamath_rrf_score}
\title{Reciprocal rank fusion score (Kamath Eq 7.1)}
\usage{
morie_kamath_rrf_score(r, k = 60)
}
\arguments{
\item{r}{1-based rank(s).}
\item{k}{Damping constant.}
}
\description{
Score_RRF = 1/(r + k) per 1-based rank, SUMMED across rankings, with Cormack et al.\'s damping k = 60 as the book\'s default. Mirrors morie.fn.km110 (ported by the lead; the agent shard missed this module and kmfait).
}
\value{
A list with \code{estimate}, \code{scores}, \code{ranks}, \code{k}, \code{n}, \code{method}.
}
\keyword{internal}
