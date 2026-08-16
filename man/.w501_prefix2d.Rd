% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w501_prefix2d}
\alias{.w501_prefix2d}
\title{.w501_prefix2d}
\usage{
.w501_prefix2d(D)
}
\arguments{
\item{D}{A matrix; indexed by row and column.}
}
\description{
Part of the w501_changepoint_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{P}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.w501_prefix2d(M)
}
