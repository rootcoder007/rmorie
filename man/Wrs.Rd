% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Wrs}
\alias{Wrs}
\title{Wilcoxon rank-sum W_N -- Sec. 8.2, pp. 290-291}
\usage{
Wrs(x, y)
}
\description{
Wilcoxon rank-sum W_N -- Sec. 8.2, pp. 290-291
}
\value{
A list with \code{statistic}, \code{p_value}, \code{z}, \code{p_normal}, \code{mean}, \code{var}, \code{wmin}, \code{wmax}, \code{m}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Wrs(V, V)
}
