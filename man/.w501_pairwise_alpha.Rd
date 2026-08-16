% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w501_pairwise_alpha}
\alias{.w501_pairwise_alpha}
\title{Z: matrix with observations in rows}
\usage{
.w501_pairwise_alpha(Z, alpha)
}
\arguments{
\item{Z}{A matrix; indexed by row and column.}
\item{alpha}{Numeric; combined arithmetically in the body.}
}
\description{
Part of the w501_changepoint_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{D}, as built in the body.
}
\keyword{internal}
\examples{
.w501_pairwise_alpha(Z = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), alpha = 0.5)
}
