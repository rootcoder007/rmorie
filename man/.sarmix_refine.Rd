% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarmix_refine}
\alias{.sarmix_refine}
\title{Internal: deterministic nested-grid minimiser on a rectangle. Both}
\usage{
.sarmix_refine(negll, lo1, hi1, lo2, hi2)
}
\arguments{
\item{negll}{See Usage.}
\item{lo1}{See Usage.}
\item{hi1}{See Usage.}
\item{lo2}{See Usage.}
\item{hi2}{See Usage.}
}
\description{
language arms run the identical arithmetic -- a fixed 21x21 sweep, then 7 zooms each shrinking the box by ten -- so they land on the same numbers rather than on two different optimiser trajectories.
}
\value{
The value of \code{best}, as built in the body.
}
\keyword{internal}
