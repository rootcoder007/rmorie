% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fm_second_order}
\alias{.fm_second_order}
\title{0.5 sum_f ((sum_i v_if x_i)^2 - sum_i (v_if x_i)^2): the O(kn)}
\usage{
.fm_second_order(V, x)
}
\arguments{
\item{V}{A matrix; indexed by row and column.}
\item{x}{A vector; its length is taken and its elements indexed.}
}
\description{
identity for the pairwise term, exactly equal to the naive double sum over i<j.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.fm_second_order(M, D)
}
