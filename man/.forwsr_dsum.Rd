% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.forwsr_dsum}
\alias{.forwsr_dsum}
\title{CPython\\'s builtin sum() switched to Neumaier compensated summation}
\usage{
.forwsr_dsum(v)
}
\arguments{
\item{v}{A vector; its length is taken and its elements indexed.}
}
\description{
for floats in 3.12. R\\'s sum() accumulates in long double, and a plain loop accumulates in double; neither reproduces it. The forward search orders residuals that differ in their last digits, so the summation algorithm decides which row enters next and the two arms part company about a dozen steps in. Do what CPython does.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.forwsr_dsum(V)
}
