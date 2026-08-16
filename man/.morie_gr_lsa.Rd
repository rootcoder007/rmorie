% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_lsa}
\alias{.morie_gr_lsa}
\title{Jonker-Volgenant shortest augmenting path, ported line-for-line from}
\usage{
.morie_gr_lsa(cost)
}
\arguments{
\item{cost}{Passed to \code{.morie_gr_mat}.}
}
\description{
grdetr._linear_sum_assignment. Returns 0-based rows/cols.
}
\value{
A list with \code{rows}, \code{cols}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_lsa(V)
}
