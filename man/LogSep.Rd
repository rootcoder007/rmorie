% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LogSep}
\alias{LogSep}
\title{Eq (4.59): log[y] = log[x] + log[p], for x != 0 and p != 0.  The}
\usage{
LogSep(x, p)
}
\arguments{
\item{x}{See Usage.}
\item{p}{See Usage.}
}
\description{
book states that side condition, so a zero is rejected rather than giving -Inf; a negative factor needs the complex-log route.
}
\value{
A list with \code{log_y}, \code{log_x}, \code{log_p}, \code{sum}, \code{max_difference}, \code{additive}, \code{method}.
}
\keyword{internal}
