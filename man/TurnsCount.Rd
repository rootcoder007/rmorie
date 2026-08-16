% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{TurnsCount}
\alias{TurnsCount}
\title{Section 5.6.3, Willison: a turn is a change of slope, counted only}
\usage{
TurnsCount(x, threshold = 100, window = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{threshold}{Passed to \code{.morie_rg_turns}. Defaults to \code{100}.}
\item{window}{Defaults to \code{NULL}.}
}
\description{
when the swing since the LAST COUNTED TURN exceeds the threshold (100 microvolts in the book).  Measuring against the last counted turn rather than the previous sample is what makes it robust in noise, and what separates it from counting turning points.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
TurnsCount(V)
}
