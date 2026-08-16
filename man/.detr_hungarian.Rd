% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.detr_hungarian}
\alias{.detr_hungarian}
\title{Exact minimum-cost assignment (Jonker-Volgenant shortest paths) on a}
\usage{
.detr_hungarian(cost)
}
\arguments{
\item{cost}{A matrix; indexed by row and column.}
}
\description{
rectangular cost with rows <= columns.  Returns the column per row.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.detr_hungarian(M)
}
