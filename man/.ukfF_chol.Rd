% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ukfF_chol}
\alias{.ukfF_chol}
\title{.ukfF_chol}
\usage{
.ukfF_chol(a)
}
\arguments{
\item{a}{A matrix; indexed by row and column.}
}
\description{
A step of the ukfF_mixedcase_native implementation. Called by \code{.ukfF_sigma_points}. See the file header for the source the module follows.
}
\value{
The value of \code{l}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.ukfF_chol(M)
}
