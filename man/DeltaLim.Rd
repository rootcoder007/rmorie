% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DeltaLim}
\alias{DeltaLim}
\title{Eq (3.26): delta(t) = 0.5 lim_{a->0} a |t|^(a-1).  The exponent is}
\usage{
DeltaLim(t, a)
}
\arguments{
\item{t}{See Usage.}
\item{a}{See Usage.}
}
\description{
negative for every a in (0,1), so the function diverges at t = 0 (NA, not a large finite number).  Its integral over [-L, L] is L^a, which tends to 1 as a -> 0 -- the unit-area property in the limit.
}
\value{
A list with \code{values}, \code{t}, \code{a}, \code{area_symmetric}, \code{half_width}, \code{method}.
}
\keyword{internal}
