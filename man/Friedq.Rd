% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Friedq}
\alias{Friedq}
\title{Friedman two-way ANOVA by ranks -- eqs. (12.2.8)/(12.2.12), pp. 441-445}
\usage{
Friedq(data, correct = TRUE)
}
\description{
Friedman two-way ANOVA by ranks -- eqs. (12.2.8)/(12.2.12), pp. 441-445
}
\value{
A list with \code{statistic}, \code{q_raw}, \code{s}, \code{df}, \code{p_value}, \code{rank_sums}, \code{k}, \code{n}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
Friedq(D)
}
