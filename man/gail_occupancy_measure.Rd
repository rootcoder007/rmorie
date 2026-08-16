% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gail_occupancy_measure}
\alias{gail_occupancy_measure}
\title{Empirical occupancy measure}
\usage{
gail_occupancy_measure(states, actions)
}
\arguments{
\item{states,actions}{Lists of equal length; \code{states} may   contain numeric vectors or scalars, \code{actions} anything.}
}
\description{
GAIL\'s whole framing is that imitation is occupancy-measure matching, so this is worth being able to look at directly. Returns a named numeric vector whose names are stringified \code{(state, action)} pairs and whose values are the empirical frequencies over the supplied samples.
}
\value{
Named numeric vector of pair frequencies.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
gail_occupancy_measure(V, V)
}
