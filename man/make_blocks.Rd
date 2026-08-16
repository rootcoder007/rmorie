% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{make_blocks}
\alias{make_blocks}
\title{make_blocks}
\usage{
make_blocks(n_markers, chromosomes = NULL, block_size = 1000)
}
\arguments{
\item{n_markers}{See Usage.}
\item{chromosomes}{Defaults to \code{NULL}.}
\item{block_size}{Defaults to \code{1000}.}
}
\description{
Part of the regmlm_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{blocks}, as built in the body.
}
\references{
Mbatchou, J., et al. (2021) "Computationally efficient whole-genome
regression for quantitative and binary traits", Nature Genetics
53(7), 1097-1103, doi:10.1038/s41588-021-00870-7.

Hoerl, A. E. & Kennard, R. W. (1970) "Ridge Regression: Biased
Estimation for Nonorthogonal Problems", Technometrics 12(1), 55-67.

Yang, J., et al. (2014) "Advantages and pitfalls in the application
of mixed-model association methods", Nature Genetics 46(2),
100-106.
}
\keyword{internal}
\examples{
make_blocks(n_markers = 5L)
}
