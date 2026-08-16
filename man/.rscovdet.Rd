% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rscovdet}
\alias{.rscovdet}
\title{Determinant of a COVARIANCE matrix, which cannot be negative.}
\usage{
.rscovdet(S)
}
\arguments{
\item{S}{Passed to \code{.rsludet}.}
}
\description{
.rsludet is a general determinant and may return a small negative number for a positive-semidefinite matrix that is singular to working precision. Every objective in this shelf is minimised over such determinants, so an unclamped negative rounding artefact wins the minimisation outright and the search returns whichever subset happened to round furthest below zero.  Clamping at zero makes the comparison pick the FIRST exactly-degenerate subset instead, which is both correct and identical in the two language arms.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.rscovdet(M)
}
