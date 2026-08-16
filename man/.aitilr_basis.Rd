% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.aitilr_basis}
\alias{.aitilr_basis}
\title{Contrast matrix of the default Egozcue (2003) sequential binary partition}
\usage{
.aitilr_basis(D)
}
\arguments{
\item{D}{number of parts.}
}
\description{
Column i holds +sqrt(i/(i+1))/i on rows 1..i, -sqrt(i/(i+1)) on row i+1 and zero elsewhere, so every column sums to zero and has unit Euclidean norm.
}
\value{
a D-by-(D-1) numeric contrast matrix.
}
\keyword{internal}
