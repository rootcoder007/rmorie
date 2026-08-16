% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Friedties}
\alias{Friedties}
\title{Tie-corrected Friedman Q -- eq. (12.2.12), p. 445}
\usage{
Friedties(data)
}
\description{
Tie-corrected Friedman Q -- eq. (12.2.12), p. 445
}
\value{
A list with \code{statistic}, \code{q_raw}, \code{tiesum}, \code{factor}, \code{s}, \code{rank_sums}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
Friedties(D)
}
