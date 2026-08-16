% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ed_prefix}
\alias{.mor_ed_prefix}
\title{P[i + 1, j + 1] = sum of D[1..i, 1..j]}
\usage{
.mor_ed_prefix(D)
}
\arguments{
\item{D}{A matrix; indexed by row and column.}
}
\description{
A step of the e_div_native implementation. Called by \code{morie_e_div}. See the file header for the source the module follows.
}
\value{
The value of \code{P}, as built in the body.
}
\keyword{internal}
