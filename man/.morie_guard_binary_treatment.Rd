% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_guard_binary_treatment}
\alias{.morie_guard_binary_treatment}
\title{Internal guard: refuse silent factor-to-numeric treatment coercion}
\usage{
.morie_guard_binary_treatment(x, col)
}
\description{
Called by the MRM pipeline (and available to every estimator): when a column that must be numeric 0/1 arrives as a factor or character, this ERRORS with the level-index explanation instead of letting \code{as.numeric(factor)} return 1, 2, ... level codes.
}
\value{
The value of \code{invisible}.
}
\keyword{internal}
