% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bacon_native}
\alias{.morie_bacon_native}
\title{Internal helper: Goodman-Bacon decomposition on a balanced panel}
\usage{
.morie_bacon_native(data, outcome, treatment, unit, time)
}
\description{
Enumerates every 2x2 timing comparison; each pair\'s estimate is the 2x2 DiD on the relevant subsample/window and its weight follows from the Frisch-Waugh-Lovell identity: proportional to the subsample size squared times the variance of the within-demeaned treatment.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
